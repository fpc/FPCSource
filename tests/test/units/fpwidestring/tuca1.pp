{ %FILES=CollationTest_NON_IGNORABLE_SHORT.txt CollationTest_SHIFTED_SHORT.txt}

program tuca1;
{ Test the Unicode Collation Algorithm (UCA) implementation
  This test uses the UCA test files :
    * CollationTest_NON_IGNORABLE_SHORT.txt
    * CollationTest_SHIFTED_SHORT.txt
  These files are in the zip archive at
    http://www.unicode.org/Public/UCA/6.2.0/CollationAuxiliary.zip
}

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Math,{$ifdef WINCE}StreamIO,{$endif}
  unicodedata, unicodeducet;

var
  TotalErrorCount : Integer = 0;

procedure DumpString(const AValue : UnicodeString);
var
  k, c : Integer;
  cp : Cardinal;
begin
  c := Length(AValue);
  k := 1;
  while (k <= c) do begin
    if (k = c) or not(UnicodeIsHighSurrogate(AValue[k])) or not(UnicodeIsLowSurrogate(AValue[k+1])) then
      cp := Word(AValue[k])
    else begin
      cp := ToUCS4(AValue[k],AValue[k+1]);
      Inc(k);
    end;
    Write(IntToHex(cp,4), ' ');
    Inc(k);
  end;
end;

procedure DumpKey(const AValue : TUCASortKey);
var
  k, c : Integer;
begin
  c := Length(AValue);
  for k := 0 to c-1 do begin
    Write(IntToHex(AValue[k],4),'|');
  end;
end;

procedure CheckContent(ADataAStream : TMemoryStream; ACollation : PUCA_DataBook);
const LINE_LENGTH        = 1024;
var
  p : PAnsiChar;
  bufferLength, bufferPos, lineLength, linePos : Integer;
  line : ansistring;
  lineCount : Integer;

  function NextLine() : Boolean;
  var
    locOldPos : Integer;
    locOldPointer : PAnsiChar;
  begin
    Result := False;
    locOldPointer := p;
    locOldPos := bufferPos;
    while (bufferPos < bufferLength) and (p^ <> #10) do begin
      Inc(p);
      Inc(bufferPos);
    end;
    if (locOldPos = bufferPos) and (p^ = #10) then begin
      lineLength := 0;
      Inc(p);
      Inc(bufferPos);
      linePos := 1;
      Result := True;
    end else  if (locOldPos < bufferPos) then begin
      lineLength := (bufferPos - locOldPos) + 1;
      Move(locOldPointer^,line[1],lineLength);
      if (p^ = #10) then begin
        Dec(lineLength);
        Inc(p);
        Inc(bufferPos);
      end;
      linePos := 1;
      Result := True;
    end;
    if Result then
      Inc(lineCount);
  end;

  procedure SkipSpace();
  begin
    while (linePos < lineLength) and (line[linePos] in [' ',#9]) do
      Inc(linePos);
  end;

  function NextToken() : ansistring;
  const C_SEPARATORS  = [';','#','.','[',']','*','@'];
  var
    k : Integer;
  begin
    SkipSpace();
    k := linePos;
    if (linePos <= lineLength) and (line[linePos] in C_SEPARATORS) then begin
      Result := line[linePos];
      Inc(linePos);
      exit;
    end;
    while (linePos <= lineLength) and not(line[linePos] in (C_SEPARATORS+[' '])) do
      Inc(linePos);
    if (linePos > k) then begin
      if (line[Min(linePos,lineLength)] in C_SEPARATORS) then
        Result := Copy(line,k,(linePos-k))
      else
        Result := Copy(line,k,(linePos-k+1));
      Result := Trim(Result);
    end else begin
      Result := '';
    end;
  end;

  function ParseLine() : UnicodeString;
  var
    locCP : Cardinal;
    s : ansistring;
    k : Integer;
  begin
    SetLength(Result,24);
    k := 0;
    while True do begin
      s := NextToken();
      if (s = '') or (s[1] = '#') then
        Break;
      Inc(k);
      if (k >= Length(Result)) then
        SetLength(Result,(2*k));
      locCP := StrToInt('$' + s);
      if (locCP <= High(Word)) then
        Word(Result[k]) := locCP
      else begin
        FromUCS4(locCP,PUnicodeChar(@Result[k])^,PUnicodeChar(@Result[k+1])^);
        Inc(k);
      end;
    end;
    SetLength(Result,k);
  end;

  procedure Prepare();
  begin
    bufferLength := ADataAStream.Size;
    bufferPos := 0;
    p := ADataAStream.Memory;
    lineLength := 0;
    SetLength(line,LINE_LENGTH);
  end;

var
  a, b : UnicodeString;
  ka, kb : TUCASortKey;
  errorCount : Integer;
begin
  errorCount := 0;
  lineCount := 0;
  SetLength(a,3);
  FromUCS4($11143,PUnicodeChar(@a[1])^,PUnicodeChar(@a[2])^);
  Word(a[3]):= $0334;
  //Word(a[2]):= $0021;
{$define stop_on_error}
  ka := ComputeSortKey(a,ACollation);
  Prepare();
  while NextLine() do begin
    a := ParseLine();
    if (a <> '') then
      Break;
  end;
  ka := ComputeSortKey(a,ACollation);
  while NextLine() do begin
    b := ParseLine();
    if (b = '') then
      Break;
    kb := ComputeSortKey(b,ACollation);
    if (CompareSortKey(kb,ka) < 0) then begin
      Inc(errorCount);
{$ifdef stop_on_error}
      Inc(TotalErrorCount,errorCount);
      WriteLn('Error #',errorCount, '; Line #',lineCount);
      Write('    s1 = ');DumpString(a);Write('    ');DumpKey(ka); WriteLn();
      Write('    s2 = ');DumpString(b);Write('    ');DumpKey(ComputeSortKey(b,ACollation)); WriteLn();
      //Write('    s2 = ');DumpString(b);Write('    ');DumpKey(kb); WriteLn();
      Exit;
{$endif stop_on_error}
    end;
    a := b;
    ka := kb;
  end;
  WriteLn('Line Count = ',lineCount);
  WriteLn('Error Count = ',errorCount);
  Inc(TotalErrorCount,errorCount);
end;

var
  stream : TMemoryStream;
  collation : PUCA_DataBook;
{$ifdef WINCE}
  fs : TFileStream;
  s : string;
{$endif WINCE}
begin
{$ifdef WINCE}
  s := ExtractFilePath(ParamStr(0))+'tuca1-log.txt';
  DeleteFile(s);
  fs := TFileStream.Create(s,fmCreate);
  AssignStream(Output,fs);
  Rewrite(Output);
  s := ExtractFilePath(ParamStr(0))+'tuca1-err.txt';
  DeleteFile(s);
  fs := TFileStream.Create(s,fmCreate);
  AssignStream(ErrOutput,fs);
  Rewrite(ErrOutput);
{$endif WINCE}
  collation := FindCollation('DUCET');
  stream := TMemoryStream.Create();
  try
    collation^.VariableWeight := TUCA_VariableKind.ucaNonIgnorable;
    stream.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'CollationTest_NON_IGNORABLE_SHORT.txt');
    stream.Position := 0;
    WriteLn('Testing CollationTest_NON_IGNORABLE_SHORT.txt ...');
    CheckContent(stream,collation);
    WriteLn();WriteLn();

    collation^.VariableWeight := TUCA_VariableKind.ucaShifted;
    stream.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'CollationTest_SHIFTED_SHORT.txt');
    stream.Position := 0;
    WriteLn('Testing CollationTest_SHIFTED_SHORT.txt ...');
    CheckContent(stream,collation);
  finally
    stream.Free();
  end;
  if (TotalErrorCount > 0) then begin
    WriteLn('Failed.');
    Halt(1);
  end;
end.
