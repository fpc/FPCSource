{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************}
{*                             TParser                                      *}
{****************************************************************************}

{!!!TSE 21.09.1998 Changed by Thomas Seban (TSE) }

const
  ParseBufSize     = 4096;

procedure BinToHex(Buffer, Text: PChar; BufSize: Integer);
begin
end;

function HexToBin(Text, Buffer: PChar; BufSize: Integer) : Integer;
begin
end;

procedure TParser.ReadBuffer;
var
  Count            : Integer;
begin
  Inc(FOrigin, FSourcePtr - FBuffer);

  FSourceEnd[0] := FSaveChar;
  Count         := FBufPtr - FSourcePtr;
  if Count <> 0 then 
  begin
    Move(FSourcePtr[0], FBuffer[0], Count);
  end;
  
  FBufPtr := FBuffer + Count;
  Inc(FBufPtr, FStream.Read(FBufPtr[0], FBufEnd - FBufPtr));
  
  FSourcePtr := FBuffer;
  FSourceEnd := FBufPtr;
  if (FSourceEnd = FBufEnd) then
  begin
    FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
    if FSourceEnd = FBuffer then 
    begin
      Error(SLineTooLong);
    end;
  end;
  FSaveChar := FSourceEnd[0];
  FSourceEnd[0] := #0;
end;


procedure TParser.SkipBlanks;
var
  Count            : Integer;
begin
  Inc(FOrigin, FSourcePtr - FBuffer);
  FSourceEnd[0] := FSaveChar;
  Count := FBufPtr - FSourcePtr;
  if Count <> 0 then Move(FSourcePtr[0], FBuffer[0], Count);
  FBufPtr := FBuffer + Count;
  Inc(FBufPtr, FStream.Read(FBufPtr[0], FBufEnd - FBufPtr));
  FSourcePtr := FBuffer;
  FSourceEnd := FBufPtr;
  if FSourceEnd = FBufEnd then
  begin
    FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
    if FSourceEnd = FBuffer then Error(SLineTooLong);
  end;
  FSaveChar := FSourceEnd[0];
  FSourceEnd[0] := #0;
end;

constructor TParser.Create(Stream: TStream);
begin
  inherited Create;
  
  FStream := Stream;
  GetMem(FBuffer, ParseBufSize);
  
  FBuffer[0]  := #0;
  FBufPtr     := FBuffer;
  FBufEnd     := FBuffer + ParseBufSize;
  FSourcePtr  := FBuffer;
  FSourceEnd  := FBuffer;
  FTokenPtr   := FBuffer;
  FSourceLine := 1;
  
  NextToken;
end;


destructor TParser.Destroy;
begin
  if Assigned(FBuffer) then
  begin
    FStream.Seek(Longint(FTokenPtr) - Longint(FBufPtr), 1);
    FreeMem(FBuffer, ParseBufSize);
  end;
  
  inherited Destroy;
end;

procedure TParser.CheckToken(T : Char);
begin
  if Token <> T then 
  begin
    case T of
      toSymbol:
        Error(SIdentifierExpected);
      toString:
        Error(SStringExpected);
      toInteger, toFloat:
        Error(SNumberExpected);
    else
      ErrorFmt(SCharExpected, [T]);
    end;
  end;
end;

procedure TParser.CheckTokenSymbol(const S: string);
begin
  if not TokenSymbolIs(S) then 
    ErrorFmt(SSymbolExpected, [S]);
end;

Procedure TParser.Error(const Ident: string);
begin
  ErrorStr(Ident);
end;

Procedure TParser.ErrorFmt(const Ident: string; const Args: array of const);
begin
  ErrorStr(Format(Ident, Args));
end;

Procedure TParser.ErrorStr(const Message: string);
begin
  raise EParserError.CreateFmt(SParseError, [Message, FSourceLine]);
end;


Procedure TParser.HexToBinary(Stream: TStream);
var
  Count            : Integer;
  Buffer           : array[0..255] of Char;
begin
  SkipBlanks;
  while FSourcePtr^ <> '}' do
  begin
    Count := HexToBin(FSourcePtr, Buffer, SizeOf(Buffer));
    if Count = 0 then Error(SInvalidBinary);
    Stream.Write(Buffer, Count);
    Inc(FSourcePtr, Count * 2);
    SkipBlanks;
  end;
  NextToken;
end;


Function TParser.NextToken: Char;
var
  I                : Integer;
  P, S             : PChar;
begin
  SkipBlanks;
  P := FSourcePtr;
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_':
      begin
        Inc(P);
        while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do Inc(P);
        Result := toSymbol;
      end;
    '#', '''':
      begin
        S := P;
        while True do
          case P^ of
            '#':
              begin
                Inc(P);
                I := 0;
                while P^ in ['0'..'9'] do
                begin
                  I := I * 10 + (Ord(P^) - Ord('0'));
                  Inc(P);
                end;
                S^ := Chr(I);
                Inc(S);
              end;
            '''':
              begin
                Inc(P);
                while True do
                begin
                  case P^ of
                    #0, #10, #13: 
                      Error(SInvalidString);
                    '''':
                      begin
                        Inc(P);
                        if P^ <> '''' then Break;
                      end;
                  end;
                  S^ := P^;
                  Inc(S);
                  Inc(P);
                end;
              end;
          else
            Break;
          end;
        FStringPtr := S;
        Result := toString;
      end;
    '$':
      begin
        Inc(P);
        while P^ in ['0'..'9', 'A'..'F', 'a'..'f'] do Inc(P);
        Result := toInteger;
      end;
    '-', '0'..'9':
      begin
        Inc(P);
        while P^ in ['0'..'9'] do Inc(P);
        Result := toInteger;
        while P^ in ['0'..'9', '.', 'e', 'E', '+', '-'] do
        begin
          Inc(P);
          Result := toFloat;
        end;
      end;
  else
    Result := P^;
    if Result <> toEOF then Inc(P);
  end;
  FSourcePtr := P;
  FToken := Result;
end;

Function TParser.SourcePos: Longint;
begin
  Result := FOrigin + (FTokenPtr - FBuffer);
end;


Function TParser.TokenComponentIdent: String;
var
  P                : PChar;
begin
  CheckToken(toSymbol);
  P := FSourcePtr;
  while P^ = '.' do
  begin
    Inc(P);
    if not (P^ in ['A'..'Z', 'a'..'z', '_']) then
      Error(SIdentifierExpected);
    repeat
      Inc(P)
    until not (P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
  end;
  FSourcePtr := P;
  Result := TokenString;
end;

Function TParser.TokenFloat: Extended;
var
  FloatError       : Integer;
  Back             : Real;
begin
  Result   := 0;

  // doesn't work, overload function not found
  // systemh.inc compiled without -S2 switch => SizeOf(Integer) = 2
  // classes.pp compiled with -S2 switch => SizeOf(Integer) = 4
  // Val(TokenString, Back, FloatError); 
  
  Val(TokenString, Back); // this works fine
  Result := Back;
end;

Function TParser.TokenInt: Longint;
begin
  Result := StrToInt(TokenString);
end;

Function TParser.TokenString: string;
var
  L                : Integer;
  StrBuf           : array[0..1023] of Char;
begin 
  if FToken = toString then begin
    L := FStringPtr - FTokenPtr 
  end else begin
    L := FSourcePtr - FTokenPtr;
  end;
  
  StrLCopy(StrBuf, FTokenPtr, L);
  Result := StrPas(StrBuf);
end;

Function TParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (Token = toSymbol) and (CompareText(S, TokenString) = 0);
end;
{
  $Log$
  Revision 1.4  1998-10-30 14:52:51  michael
  + Added format in interface
  + Some errors in parser fixed, it uses exceptions now
  + Strings now has no more syntax errors.

  Revision 1.3  1998/10/02 22:41:28  michael
  + Added exceptions for error handling

  Revision 1.2  1998/09/23 07:48:11  michael
  + Implemented by TSE

  Revision 1.1  1998/05/04 14:30:12  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

}
