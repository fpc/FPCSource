program testnfd;
{       
  This program tests the "NormalizeNFD" with the Unicode provided test file.
  The test file "NormalizationTest.txt" is to find in the Unicode Character
  Database.
}

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Math, unicodedata;

type
  TDataPartLine = record
    c1, c2, c3, c4, c5 : UCS4String;
  end;
  PDataPartLine = ^TDataPartLine;

  TDataPart = record
    Part : AnsiString;
    Lines : array of TDataPartLine;
    ActualLength : Integer;
  end;
  PDataPart = ^TDataPart;
   
const
  LINE_LENGTH         = 1024;
  DEFAULT_DATA_LINE_LENGTH = 25000;

var
  p : PAnsiChar;
  bufferLength, bufferPos, lineLength, linePos : Integer;
  line : ansistring;          
  totalErrorCount : Integer = 0;
  lineCount, errorCount : Integer;
  stream : TMemoryStream;
  part : ansistring;
  c1, c2, c3, c4, c5 : UCS4String;
  s1, s2, s3, s4, s5 : UnicodeString;
  dataList : array of TDataPart;
  dataListActualLength : Integer;
  pp, part1 : PDataPart;

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

  function ParseLine() : Integer;
  var
    t : UCS4String;
    r : array[0..23] of UCS4String;
    rc, k : Integer;
    s : ansistring;
  begin
    rc := 0;
    SetLength(c1,0);
    SetLength(c2,0);
    SetLength(c3,0);
    SetLength(c4,0);
    SetLength(c5,0);
    SetLength(t,0);
    while (rc < Length(r)) do begin
      s := NextToken();
      if (s = '#') then
        break;
      if (s = '@') then begin
        part := NextToken();
        rc := 0;
        continue;
      end;
      if (s = '') or (s[1] = '#') then
        Break;
      if (s <> ';') then begin
        k := Length(t);
        SetLength(t,(k+1));
        t[k] := StrToInt('$' + s);
      end else if (s = ';') then begin
        k := Length(t);
        SetLength(t,(k+1));
        t[k] := 0;
        r[rc] := Copy(t);
        SetLength(t,0);
        Inc(rc);
      end;
    end;
    c1 := r[0]; s1 := UCS4StringToUnicodeString(c1);
    c2 := r[1]; s2 := UCS4StringToUnicodeString(c2);
    c3 := r[2]; s3 := UCS4StringToUnicodeString(c3);
    c4 := r[3]; s4 := UCS4StringToUnicodeString(c4);
    c5 := r[4]; s5 := UCS4StringToUnicodeString(c5);
    Result := rc;
  end;

  procedure AddDataLine();
  var
    k : Integer;
    p : PDataPart;
    pline : PDataPartLine;
  begin
    p := nil;
    for k := Low(dataList) to High(dataList) do begin
      if (dataList[k].Part = part) then begin
        p := @dataList[k];
        break;
      end;
    end;
    if (p = nil) then begin
      k := dataListActualLength;
      if (k >= Length(dataList)) then
        SetLength(dataList,(k+5)); 
      dataListActualLength := k+1;
      p := @dataList[k];
      p^.Part := part;
    end;

    k := p^.ActualLength;
    if (k >= Length(p^.Lines)) then
      SetLength(p^.Lines,(k+DEFAULT_DATA_LINE_LENGTH));
    pline := @p^.Lines[k];
    pline^.c1 := c1;
    pline^.c2 := c2;
    pline^.c3 := c3;
    pline^.c4 := c4;
    pline^.c5 := c5; 
    p^.ActualLength := k+1;
    c1 := nil;
    c2 := nil;
    c3 := nil;
    c4 := nil;
    c5 := nil;
  end;

  function IsInPart(ACodePoint : UCS4Char; APart : PDataPart) : boolean;
  var
    k : Integer;
    pline : PDataPartLine;
  begin
    pline := @APart^.Lines[0];
    for k := 0 to APart^.ActualLength-1 do begin
      if (Length(pline^.c1) = 2) and (pline^.c1[0] = ACodePoint) then
        exit(True);
      Inc(pline);
    end;
    Result := False;
  end;

  procedure Prepare();
  begin
    bufferLength := stream.Size;
    bufferPos := 0;
    p := stream.Memory;
    lineLength := 0;
    SetLength(line,LINE_LENGTH);
    SetLength(dataList,10);
    dataListActualLength := 0;
  end;

  procedure TestLines();
  var
    lineErrors : Integer;
  begin    
    while NextLine() do begin
      if (ParseLine() < 5) then
        continue;
      AddDataLine();
      lineErrors := 0;
      //c3 ==  toNFD(c1) ==  toNFD(c2) ==  toNFD(c3)
      if (NormalizeNFD(s1) <> s3) then
        lineErrors := lineErrors+1;
      if (NormalizeNFD(s2) <> s3) then
        lineErrors := lineErrors+1;
      if (NormalizeNFD(s3) <> s3) then
        Inc(errorCount);
      //c5 ==  toNFD(c4) ==  toNFD(c5)
      if (NormalizeNFD(s4) <> s5) then
        lineErrors := lineErrors+1;
      if (NormalizeNFD(s5) <> s5) then
        lineErrors := lineErrors+1;
      if (lineErrors <> 0) then
        errorCount := errorCount+lineErrors;
    end;
  end;
       
{$IFDEF ALL_CODE_POINTS}
  procedure TestBmpCodePoints();
  var
    cp : Word;
    s : UnicodeString;
    pu : PUC_Prop;
  begin
    SetLength(s,1);
    for cp := Low(Word) to High(Word) do begin
      pu := GetProps(cp);
      if (pu^.Category <> UGC_Unassigned) and (pu^.Category <> UGC_Surrogate) and
         not(IsInPart(cp,part1))
      then begin
        //X == toNFC(X) == toNFD(X) == toNFKC(X) == toNFKD(X)  
        PWord(@s[1])^ := cp;
        if (NormalizeNFD(s) <> s) then
          errorCount := errorCount+1;
      end;
    end;
  end;       

  procedure TestOBmpCodePoints();
  var
    cp : UCS4Char;
    s : UnicodeString;
    pu : PUC_Prop;
  begin
    SetLength(s,2);
    s[1] := 'a'; s[2] := 'a';
    for cp := High(Word)+1 to MAX_LEGAL_UTF32 do begin
      pu := GetProps(cp);
      if (pu^.Category <> UGC_Unassigned) and (pu^.Category <> UGC_Surrogate) and
         not(IsInPart(cp,part1))
      then begin
        //X == toNFC(X) == toNFD(X) == toNFKC(X) == toNFKD(X)
        FromUCS4(cp,s[1],s[2]);
        if (NormalizeNFD(s) <> s) then
          errorCount := errorCount+1;
      end;
    end;
  end;
{$ENDIF ALL_CODE_POINTS}

var
  i, c : Integer;
begin
  errorCount := 0;
  lineCount := 0;
  stream := TMemoryStream.Create();
  try
    stream.LoadFromFile('NormalizationTest.txt');
    Prepare();
    // Direct tests specified in NormalizationTest.txt
    TestLines();
    part1 := nil;
    c := 0;
    for i := 0 to dataListActualLength-1 do begin
      pp := @dataList[i];
      if (Length(pp^.Lines) <> pp^.ActualLength) then
        SetLength(pp^.Lines,pp^.ActualLength);
      c := c+pp^.ActualLength;
      if SameText(pp^.Part,'Part1') then
        part1 := pp;
    end;
    if (part1 = nil) then
      raise Exception.Create('"Part1" not found !');  
{ $DEFINE ALL_CODE_POINTS}
{$IFDEF ALL_CODE_POINTS}
    // Tests for BMP Codepoints not is PART1
    TestBmpCodePoints();     
    // Tests for BMP Codepoints not is PART1
    TestOBmpCodePoints();   
{$ENDIF ALL_CODE_POINTS}
    WriteLn('Line Count = ',lineCount);
    WriteLn('Actual Test Line Count = ',c);
    WriteLn('Error Count = ',errorCount);
    Inc(totalErrorCount,errorCount);
  finally
    stream.Free();
  end; 
  if (totalErrorCount > 0) then begin
    WriteLn('Failed.');
    Halt(1);
  end;
end.

