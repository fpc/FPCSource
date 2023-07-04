{ **********************************************************************
  This file is part of the Free Component Library

  PDF Scanner
  Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  **********************************************************************}
unit fppdfscanner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fppdfobjects, fppdfsource;

Const
  PDFStringLengthDelta = 100;


Type
  TPDFCharacterClass = (ccWhitespace,ccDelimiter,ccRegular);
  TPDFContext = (cNone,cObj,cXRef,cTrailer);

  { EPDFScanner }

  EPDFScanner = Class(EPDF)
  private
    FErrorNumber: Integer;
    FPosition: Int64;
  Public
    Property ErrorNumber : Integer Read FErrorNumber Write FErrorNumber;
    Property Position : Int64 Read FPosition Write FPosition;
  end;

  { TPDFScanner }

  TPDFScanner = Class
  Private
    FLastTokenPosition: Int64;
    FSource : TPDFSourceStream;
    FUnget : Array[1..100] of TPDFToken;
    FunGetLen : Integer;
    function GetPosition: Int64;
    function GetStream: TStream;
  protected
    Procedure DoError(const Nr : Integer; Const Msg : string);
    Procedure DoError(const Nr : Integer; Const Fmt : string; Const Args : Array of const);
    function ParseHexString(const aStartByte: Byte): RawByteString; virtual;
    function ParseString(const aStartByte: Byte): RawByteString; virtual;
    Property PDF : TPDFSourceStream Read FSource;
    Function DoGetToken : TPDFToken; virtual;
  Public
    Constructor Create(aFile : TStream; aBufSize : Cardinal = PDFDefaultBufferSize); virtual;
    Destructor Destroy; override;
    Class Function CharacterClass(aByte : Byte) : TPDFCharacterClass;
    Function GetToken(aAllowWhiteSpace : boolean = True) : TPDFToken;
    Function CopyBytes(B : TBytes; aCount : Integer) : Integer;
    Procedure ReadStreamData(S : TStream);
    Procedure Unget(aToken : TPDFToken);
    Procedure Reposition(aPosition : Int64; ReadForward : Boolean = True);
    Function CompareBytes(aString : RawByteString; DoUnget : Boolean = False) : Boolean;
    Function CompareBytesBackwards(aString : RawByteString; DoUnget : Boolean = False) : Boolean;
    function FindByte(aByte: Byte): Int64;
    function GetTillByte(aByte: Byte): RawByteString;
    Function FindByteBackwards(aByte : Byte) : Int64;
    Function FindBytes(aString : RawByteString; DoUnget : Boolean = False) : Int64;
    Function FindBytesBackward(aString : RawByteString; DoUnget : Boolean = False) : Int64;
    Property Stream : TStream Read GetStream;
    Property Source : TPDFSourceStream Read FSource;
    Property LastTokenPosition : Int64 Read FLastTokenPosition;
    Property Position : Int64 Read GetPosition;
  end;

Const
  senEOFWhileScanningString = 1;
  senInvalidOctalCharacter = 2;
  senEOFWhileScanningEOD = 3;
  senInvalidCharWhileScanningEOD = 4;
  senInvalidHexString = 5;

implementation

resourcestring
  SErrEOFWhileScanningString = 'EOF while scanning string literal';
  SErrInvalidOctalCharacter = 'Invalid octal character: "%s"';
  SErrEOFWhileScanningEOD = 'EOF while scanning >>';
  SErrInvalidCharWhileScanningEOD = 'Invalid character "%s" while scanning >>';
  SErrInvalidHexString = 'Invalid hex string: %s';

constructor TPDFScanner.Create(aFile: TStream; aBufSize: Cardinal);

begin
  FSource:=TPDFSourceStream.Create(aFile,aBufSize);
  FSource.FillBufferForwardAt(0);
end;

destructor TPDFScanner.Destroy;
begin
  FreeAndNil(FSource);
  inherited Destroy;
end;

class function TPDFScanner.CharacterClass(aByte: Byte): TPDFCharacterClass;

begin
  Result:=ccRegular;
  if aByte in [0,9,10,12,13,32] then
    result:=ccWhitespace
  else if aByte in [$28,$29,$3C,$3E,$5B,$5D,$7B,$7D,$2F,$25] then
    Result:=ccDelimiter
end;

function TPDFScanner.GetToken(aAllowWhiteSpace: boolean = True): TPDFToken;
begin
  Repeat
    Result:=DoGetToken;
  until aAllowWhiteSpace or (Result.TokenType<>ptWhiteSpace) ;
  FLastTokenPosition:=Result.TokenPos;
end;

function TPDFScanner.CopyBytes(B: TBytes; aCount: Integer): Integer;

begin
  If FUngetLen>0 then
    Raise EPDFScanner.Create('Cannot copy bytes with unget data present');
  Result:=FSource.CopyBytes(B,aCount);
end;

procedure TPDFScanner.ReadStreamData(S: TStream);

  Function IsEndOfStream(var aToken : TPDFToken) : Boolean;

  begin
    Result:=(aToken.TokenType=ptKeyword) and (aToken.TokenData='endstream');
  end;

Var
  lToken : TPDFToken;



begin
  lToken:=GetToken;
  While Not IsEndOfStream(lToken) do
    begin
    S.WriteBuffer(lToken.TokenData[1],Length(lToken.TokenData));
    lToken:=GetToken;
    end;
  if IsEndOfStream(lToken) then
    Unget(lToken);
end;

procedure TPDFScanner.Unget(aToken: TPDFToken);
begin
  Inc(FunGetLen);
  FUnget[FunGetLen]:=aToken;
end;

procedure TPDFScanner.Reposition(aPosition: Int64; ReadForward : Boolean = True);

Var
  Res : Boolean;

begin
  if aPosition<0 then
    aPosition:=FSource.StreamSize+aPosition;
  if ReadForward then
    Res:=FSource.FillBufferForwardAt(aPosition)
  else
    Res:=FSource.FillBufferBackwardAt(aPosition);
  FunGetLen:=0;
  if not Res then
    Raise EPDFSCanner.CreateFmt('Invalid position : %d',[aPosition]);
end;

function TPDFScanner.CompareBytes(aString: RawByteString; DoUnget : Boolean = False): Boolean;

Var
  I,Len : Integer;
  Bytes : PByte;

begin
  Result:=True;
  Len:=Length(aString);
  Bytes:=PByte(PChar(aString));
  I:=0;
  While Result and (I<len) do
    begin
    Result:=(FSource.GetByte=Bytes^);
    Inc(I);
    Inc(Bytes);
    end;
  if DoUnGet then
    begin
    While I>0 do
      begin
      FSource.Previous;
      Dec(I);
      end;
    end;
end;

function TPDFScanner.CompareBytesBackwards(aString: RawByteString;
  DoUnget: Boolean): Boolean;
Var
  I,Len : Integer;
  Bytes : PByte;

begin
  Result:=True;
  Len:=Length(aString);
  Bytes:=PByte(PChar(@aString[Len]));
  I:=Len;
  While Result and (I>0) do
    begin
    Result:=(FSource.GetByte(True)=Bytes^);
    Dec(I);
    Dec(Bytes);
    end;
  If Result then // Put back on current position
    FSource.Next;
  if DoUnGet then
    begin
    While I<Len do
      begin
      FSource.Next;
      Inc(I);
      end;
    end;
end;

function TPDFScanner.FindByte(aByte : Byte) : Int64;

begin
  Result:=-1;
  While (Result=-1) and not FSource.IsEOF do
    begin
    if (aByte=FSource.Cursor^) then
      Result:=FSource.Position
    else
      FSource.Next;
    end;
end;

function TPDFScanner.GetTillByte(aByte: Byte): RawByteString;


Var
  I,Len : Integer;
  aCurrent : Byte;


begin
  Result:='';
  if FunGetLen>0 then
    Raise EPDFScanner.Create('Cannot get till byte when unget tokens are present');
  Len:=100;
  SetLength(Result,Len);
  I:=0;

  aCurrent:=Source.Cursor^;
  While (Not FSource.IsEOF) and (aCurrent<>aByte) do
    begin
    Inc(I);
    if I>Len then
      begin
      Len:=Len+PDFStringLengthDelta;
      SetLength(Result,Len);
      end;
    Result[i]:=AnsiChar(aCurrent);
    Source.Next;
    aCurrent:=Source.Cursor^;
    end;
  if FSource.IsEOF then
    Raise EPDFScanner.Create('EOF encountered while scanning for byte');
  Source.Next;
  SetLength(Result,I);
end;

function TPDFScanner.FindByteBackwards(aByte: Byte): Int64;

{Var
  C1,C2 : Char;
}
begin
  Result:=-1;
  While (Result=-1) and Not FSource.isBOF do
    begin
 //   C1:=Char(aByte);
//    C2:=Char(FSource.Cursor^);
    if (aByte=FSource.Cursor^) then
      Result:=FSource.Position
    else
      FSource.Previous;
    end;
end;

function TPDFScanner.FindBytes(aString: RawByteString; DoUnget : Boolean = False): Int64;

Var
  B : Byte;

begin
  Result:=-1;
  B:=Ord(aString[1]);
  While (Result=-1) and not FSource.IsEOF do
    begin
    Result:=FindByte(B);
    if Result<>-1 then
      begin
      if not CompareBytes(aString,DoUnget) then
        begin
        Result:=-1;
        FSource.Next;
        end;
      end
    end;
end;

function TPDFScanner.FindBytesBackward(aString: RawByteString; DoUnget : Boolean = False): Int64;
Var
  B : Byte;
  len : Integer;

begin
  Result:=-1;
  Len:=Length(aString);
  B:=Ord(aString[Len]);
  While (Result=-1) and Not FSource.isBOF do
    begin
    Result:=FindByteBackwards(B);
    if Result<>-1 then
      begin
      if CompareBytesBackwards(aString,DoUnget) then
        Dec(Result,Length(aString)-1)
      else
        begin
        Result:=-1;
        FSource.Previous;
        end;
      end;
    end;
end;

function TPDFScanner.GetStream: TStream;
begin
  Result:=FSource.Stream;
end;

function TPDFScanner.GetPosition: Int64;
begin
  Result:=FSource.Position;
end;

function TPDFScanner.DoGetToken: TPDFToken;


Var
  CurrentToken : RawByteString;
  CharPos : Integer;

  Procedure AddToToken(aByte : Byte);
  var
    L : Integer;

  begin
    Inc(CharPos);
    L:=Length(CurrentToken);
    if CharPos>L then
      SetLength(CurrentToken,L*2);
    CurrentToken[CharPos]:=Char(aByte);
  end;

  Procedure SetToken(aByte : Byte; aType : TPDFTokenType); // inline;

  begin
    CurrentToken:=Char(aByte);
    CharPos:=1;
    Result.TokenType:=aType;
  end;

Var
  aByte : Byte;
  aChar : Char absolute aByte; // For debugging
  isNumeric : Boolean;
  I : Integer;

begin
  if FunGetLen>0 then
    begin
    Result:=FUnget[FunGetLen];
    Dec(FunGetLen);
    Exit;
    end;
  SetLength(CurrentToken,10);
  CharPos:=0;
  Result:=Default(TPDFToken);
  Result.TokenPos:=FSource.Position;
  if FSource.isEOF then
    Exit;
  aByte:=FSource.GetByte;
  // C:=Char(aByte);
  //  Writeln('Examining: "'+C+'"');
  Case CharacterClass(aByte) of
    ccWhitespace :
      begin
      result.TokenType:=ptWhiteSpace;
      While (not FSource.isEOF) and (CharacterClass(aByte)=ccWhiteSpace) do
        begin
        AddToToken(aByte);
        aByte:=FSource.GetByte;
        end;
      if not FSource.IsEOF then
        FSource.Previous
      else if (CharacterClass(aByte)=ccWhiteSpace) then
        AddToToken(aByte);
      end;
    ccRegular:
      begin
      Result.TokenType:=ptKeyword;
      While not FSource.isEOF and (CharacterClass(aByte)=ccRegular) do
        begin
        AddToToken(aByte);
        aByte:=FSource.GetByte;
        end;
      if not FSource.IsEOF then
        FSource.Previous
      else if (CharacterClass(aByte)=ccRegular) then
        AddToToken(aByte);
      I:=1;
      isNumeric:=True;
      While IsNumeric and (I<=CharPos) do
        begin
        isNumeric:=CurrentToken[i] in ['0'..'9','+','-','.'];
        Inc(I);
        end;
      if IsNumeric then
        Result.TokenType:=ptNumber;
      end;

    ccDelimiter:
      begin
      case aByte of
        $3C: // <
           begin
           Result.TokenType:=ptshl;
           AddToToken(aByte);
           if Not FSource.IsEOF then
             begin
             aByte:=FSource.GetByte;
             if aByte=$3C then
               AddToToken(aByte)
             else
               begin
               Result.TokenType:=ptHexString;
               CurrentToken:=ParseHexString(aByte);
               CharPos:=Length(CurrentToken);
               end;
             end;
           end;
        $3E: // >
           begin
           AddToToken(aByte);
           if FSource.IsEOF then
             DoError(senEOFWhileScanningEOD,SErrEOFWhileScanningEOD);
           aByte:=FSource.GetByte;
           if aByte=$3E then
             begin
             Result.TokenType:=ptShr;
             AddToToken(aByte);
             end
           else
             DoError(senInvalidCharWhileScanningEOD,SErrInvalidCharWhileScanningEOD,[aChar]);
           end;
        $25: // %
          begin
          Result.TokenType:=ptComment;
          While Not FSource.IsEOF do
            begin
            AddToToken(aByte);
            if (aByte in [10,13]) then
              begin
              if not FSource.IsEOF then
                aByte:=FSource.GetByte;
              Break;
              end;
            if FSource.IsEOF then
              aByte:=0
            else
              aByte:=FSource.GetByte;
            end;
          if FSource.IsEOF then
            begin
            if aByte<>0 then
              AddToToken(aByte)
            end
          else if aByte=10 then
            AddToToken(aByte)
          else
            FSource.Previous;
          end;
        $2F: // /
          begin
          Result.TokenType:=ptName;
          AddToToken(aByte);
          aByte:=FSource.GetByte;
          While (not FSource.IsEOF) and (CharacterClass(aByte) = ccRegular) do
            begin
            AddToToken(aByte);
            aByte:=FSource.GetByte;
            end;
          FSource.Previous;
          end;
        $5B: SetToken(aByte,ptSquareOpen);  // [
        $5D: SetToken(aByte,ptSquareClose); // ]
        $7B: SetToken(aByte,ptCurlyOpen);   // {
        $7D: SetToken(aByte,ptCurlyClose);  // }
        $28:   // (
          begin
          Result.TokenType:=ptString;
          CurrentToken:=ParseString(aByte);
          CharPos:=Length(CurrentToken);
          end;
        end;
      end; // Case ccdelimiter
  end; // Case CharacterClass(aByte)
  SetLength(CurrentToken,CharPos);
  Result.TokenData:=CurrentToken;
  SetCodePage(Result.TokenData,1252,False);

end;

function HexToBin(HexValue, BinValue: PChar; BinBufSize: Integer): Integer;
// more complex, have to accept more than bintohex
// A..F    1000001
// a..f    1100001
// 0..9     110000
var i,j,h,l : integer;

begin
  Result:=0;
  i:=binbufsize;
  while (i>0) and (hexvalue^<>#0) do
    begin
    While (hexvalue^ IN [' ',#10,#13,#12,#9]) do
      inc(hexvalue);
    if hexvalue^=#0 then
       break;
    if hexvalue^ IN ['A'..'F','a'..'f'] then
      h:=((ord(hexvalue^)+9) and 15)
    else if hexvalue^ IN ['0'..'9'] then
      h:=((ord(hexvalue^)) and 15)
    else
      Exit(-1);
    inc(hexvalue);
    While (hexvalue^ IN [' ',#10,#13,#12,#9]) do
      inc(hexvalue);
    if hexvalue^ IN ['A'..'F','a'..'f'] then
      l:=(ord(hexvalue^)+9) and 15
    else if hexvalue^ IN ['0'..'9'] then
      l:=(ord(hexvalue^)) and 15
    else
      Exit(-1);
    j := l + (h shl 4);
    inc(hexvalue);
    binvalue^:=chr(j);
    inc(binvalue);
    dec(i);
    end;
  result:=binbufsize-i;
end;


function TPDFScanner.ParseHexString(const aStartByte: Byte): RawByteString;

Var
  aValue : RawBytestring;
  lRes,lRawlen : Integer;

begin
  Result:='';
  aValue:=Char(aStartByte)+GetTillByte(Ord('>'));
  lRawlen:=Length(aValue) div 2;
  SetLength(Result,lRawLen);
  lRes:=HexToBin(PChar(aValue),PChar(Result),lRawLen);
  if lRes=-1 then
    DoError(senInvalidHexString,SErrInvalidHexString,[aValue]);
  SetLength(Result,lRes);
  // We're still on >, move to next character
  if not Source.Cursor^=Ord('>') then
    Raise Exception.Create('not on >');
end;


function TPDFScanner.ParseString(const aStartByte: Byte): RawByteString;

Const
  cOpen = ord('(');
  cClose = ord(')');
  cEscape = Ord('\');

Var
  CurrentToken : RawByteString;
  CharPos : Integer;
  lOpenCount : Integer;
  aByte,aByte2 : Byte;
  aChar : Char absolute aByte;
  aChar2 : Char absolute aByte2;
  aChar3,aChar4 : Char;
  aOctal : integer;


  Procedure AddToToken(cByte : Byte);
  var
    L : Integer;

  begin
    Inc(CharPos);
    L:=Length(CurrentToken);
    if CharPos>L then
      SetLength(CurrentToken,L*2);
    CurrentToken[CharPos]:=Char(cByte);
  end;


begin
  CharPos:=0;
  CurrentToken:=Default(RawBytestring);
  SetLength(CurrentToken,10);
  lOpenCount:=1;
  repeat
    aByte:=FSource.GetByte;
    While not (FSource.isEOF or (aByte in [cOpen,cClose,cEscape])) do
      begin
      AddToToken(aByte);
      aByte:=FSource.GetByte;
      end;
    Case aByte of
    cEscape :
      begin
      if FSource.IsEOF then
        DoError(senEOFWhileScanningString,SErrEOFWhileScanningString);
      aByte2:=FSource.GetByte();
      case aChar2 of
        #10 : ; // Ignore
        'n' : AddToToken(10);
        'r' : AddToToken(13);
        't' : AddToToken(9);
        'b' : AddToToken(8);
        'f' : AddToToken(12);
        '(' : AddToToken(ord('('));
        ')' : AddToToken(ord(')'));
        '\' : AddToToken(ord('\'));
        '0'..'9':
            begin
            if FSource.IsEOF then
              DoError(senEOFWhileScanningString,SErrEOFWhileScanningString);
            aChar3:=Char(FSource.GetByte());
            if FSource.IsEOF then
              DoError(senEOFWhileScanningString,SErrEOFWhileScanningString);
            aChar4:=Char(FSource.GetByte());
            aOctal:=StrToIntDef('&'+aChar2+aChar3+aChar4,-1);
            if (aOctal=-1) or (aOctal>=256) then
              DoError(senInvalidOctalCharacter,SErrInvalidOctalCharacter,[aChar2+aChar3+aChar4]);
            AddToToken(aOctal and $FF)
            end
      else
        // Ignore
      end;
      end;
    cOpen:
      begin
      inc(lOpenCOunt);
      AddToToken(cOpen);
      end;
    cClose:
      begin
      Dec(lOpenCOunt);
      if lOpenCount>=1 then
        AddToToken(cClose);
      end;
    end;
  until (lOpenCount<=0) or FSource.IsEOF;
  if lOpenCount>0 then
    DoError(senEOFWhileScanningString,SErrEOFWhileScanningString);
  SetLength(CurrentToken,CharPos);
  Result:=CurrentToken;
  SetCodePage(Result,1252,False);
end;

procedure TPDFScanner.DoError(const Nr: Integer; const Msg: string);

Var
  Err : EPDFScanner;

begin
  Err:=EPDFScanner.Create(Msg);
  Err.ErrorNumber:=Nr;
  Err.Position:=FSource.Position;
  Raise Err;
end;

procedure TPDFScanner.DoError(const Nr: Integer; const Fmt: string;
  const Args: array of const);
begin
  DoError(Nr,Format(Fmt,Args));
end;


end.

