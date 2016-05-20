{   Parser of the CLDR collation tailoring files.
    This parser handle the textual syntax for CLDR version > 23

    Copyright (c) 2014,2015 by Inoussa OUEDRAOGO

    The source code is distributed under the Library GNU
    General Public License with the following modification:

        - object files and libraries linked into an application may be
          distributed without source code.

    If you didn't receive a copy of the file COPYING, contact:
          Free Software Foundation
          675 Mass Ave
          Cambridge, MA  02139
          USA

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit cldrtxt;

{$mode objfpc}{$H+}
{$TypedAddress on}

interface

uses
  Classes, SysUtils,
  cldrhelper, helper;

  procedure ParseInitialDocument(ASequence : POrderedCharacters; ADoc : TCustomMemoryStream);overload;
  procedure ParseInitialDocument(ASequence : POrderedCharacters; AFileName : string);overload;

  function ParseStatement(
        AData          : PAnsiChar;
        AStartPosition,
        AMaxLen        : Integer;
        AStatement     : PReorderSequence;
    var ANextPos,
        ALineCount     : Integer
  ) : Boolean;

implementation
uses
  unicodedata;

const
  s_BEFORE = 'before';

function String2UnicodeCodePointArray(const AValue : UTF8String): TUnicodeCodePointArray;
var
  u4str : UCS4String;
  k : Integer;
begin
  if (Length(AValue) = 0) then
    exit(nil);
  if (Length(AValue) = 1) then begin
    SetLength(Result,1);
    Result[0] := Ord(AValue[1])
  end else begin
    u4str := UnicodeStringToUCS4String(UTF8Decode(AValue));
    k := Length(u4str) - 1; // remove the last #0
    SetLength(Result,k);
    for k := 0 to k - 1 do
      Result[k] := u4str[k];
  end;
end;

function TryStringToReorderWeigthKind(
  const AStr    : UTF8String;
  out   AResult : TReorderWeigthKind
) : Boolean;
begin
  Result := True;
  if (AStr = '=') then
    AResult := TReorderWeigthKind.Identity
  else if (AStr = '<') or (AStr = '>') then
    AResult := TReorderWeigthKind.Primary
  else if (AStr = '<<') or (AStr = '>>') then
    AResult := TReorderWeigthKind.Secondary
  else if (AStr = '<<<') or (AStr = '>>>') then
    AResult := TReorderWeigthKind.Tertiary
  else begin
    AResult := TReorderWeigthKind.Identity;
    Result := False;
  end;
end;

function ParseStatement(
      AData          : PAnsiChar;
      AStartPosition,
      AMaxLen        : Integer;
      AStatement     : PReorderSequence;
  var ANextPos,
      ALineCount     : Integer
) : Boolean;
const
  LINE_LENGTH        = 1024;
var
  p : PAnsiChar;
  bufferLength, bufferPos, lineLength, linePos, lineIndex : Integer;
  line : UTF8String;
  statement : PReorderSequence;
  elementActualCount : Integer;
  specialChararter : Boolean;
  historyItemIndex : Integer;
  historyItems : array[0..31] of record
    p              : PAnsiChar;
    bufferLength,
    bufferPos,
    lineLength,
    linePos,
    lineIndex      : Integer;
    line           : UTF8String;
  end;

  procedure SaveState();
  begin
    if (historyItemIndex >= High(historyItems)) then
      raise Exception.Create('History buffer is full.');
    historyItemIndex := historyItemIndex+1;
    historyItems[historyItemIndex].p := p;
    historyItems[historyItemIndex].bufferLength := bufferLength;
    historyItems[historyItemIndex].bufferPos := bufferPos;
    historyItems[historyItemIndex].lineLength := lineLength;
    historyItems[historyItemIndex].linePos := linePos;
    historyItems[historyItemIndex].lineIndex := lineIndex;
    historyItems[historyItemIndex].line := line;
  end;

  procedure RestoreState();
  begin
    if (historyItemIndex < 0) then
      raise Exception.Create('History buffer is empty.');
    p := historyItems[historyItemIndex].p;
    bufferLength := historyItems[historyItemIndex].bufferLength;
    bufferPos := historyItems[historyItemIndex].bufferPos;
    lineLength := historyItems[historyItemIndex].lineLength;
    linePos := historyItems[historyItemIndex].linePos;
    lineIndex := historyItems[historyItemIndex].lineIndex;
    line := historyItems[historyItemIndex].line;
    historyItemIndex := historyItemIndex-1;
  end;

  procedure DiscardState();
  begin
    if (historyItemIndex < 0) then
      raise Exception.Create('History buffer is empty.');
    historyItemIndex := historyItemIndex-1;
  end;

  function CurrentLine() : UTF8String; inline;
  begin
    Result := Copy(line,1,lineLength);
  end;

  function NextLine() : Boolean;
  var
    locOldPos : Integer;
    locOldPointer : PAnsiChar;
  begin
    Result := False;
    if (p^ = #10) then begin
      Inc(p);
      Inc(bufferPos);
    end;
    locOldPos := bufferPos;
    locOldPointer := p;
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
      lineLength := (bufferPos - locOldPos);
      if (lineLength >= Length(line)) then
        SetLength(line,(2*lineLength));
      Move(locOldPointer^,line[1],lineLength);
      {if (p^ = #10) then begin
        //Dec(lineLength);
        Inc(p);
        Inc(bufferPos);
      end;}
      linePos := 1;
      Result := True;
    end;
    if Result and (locOldPos < bufferPos) then
      lineIndex := lineIndex+1;
  end;

  procedure CheckLineLength(const ALength : Integer);
  begin
    if (ALength > lineLength) then
      raise Exception.CreateFmt('Unexpected end of line : "%s".',[CurrentLine()]);
  end;

  function ReadChar(out AResult : UTF8String) : Boolean;
  var
    k : Integer;
    us : UnicodeString;
  begin
    AResult := '';
    Result := False;
    if (linePos > lineLength) then
      exit;
    {if CharInSet(line[linePos],['#','=','&','[',']']) then begin
      AResult := line[linePos];
      Inc(linePos);
      exit(True);
    end;}
    if (line[linePos] <> '\') then begin
      AResult := line[linePos];
      Inc(linePos);
      exit(True);
    end;
    CheckLineLength(linePos+1);
    Inc(linePos);
    case line[linePos] of
      '''': begin
              AResult := '\';
              exit(True);
            end;
      {'\' : begin
              AResult := '\';
              exit(True);
            end;}
      'u' : begin
              CheckLineLength(linePos+4);
              AResult := '$'+Copy(line,(linePos+1),4);
              if not TryStrToInt(AResult,k) then
                raise Exception.CreateFmt('Hexadecimal Integer expected but found "%s", line = "%s".',[AResult,CurrentLine()]);
              SetLength(us,1);
              us[1] := UnicodeChar(k);
              AResult := UTF8Encode(us);
              Inc(linePos,5);
              exit(True);
            end;
      'U' : begin
              CheckLineLength(linePos+8);
              AResult := '$'+Copy(line,(linePos+1),8);
              if not TryStrToInt(AResult,k) then
                raise Exception.CreateFmt('Hexadecimal Integer expected but found "%s".',[AResult]);
              if (k > High(Word)) then begin
                SetLength(us,2);
                FromUCS4(k,us[1],us[2]);
                if (Ord(us[2]) = 0) then
                  SetLength(us,1);
              end else begin
                SetLength(us,1);
                us[1] := UnicodeChar(k);
              end;
              AResult := UTF8Encode(us);
              Inc(linePos,9);
              exit(True);
            end;
      else
        raise Exception.CreateFmt('Invalide escaped string "%s", at %d position.',[CurrentLine(),linePos]);
    end;
  end;

  function ReadQuotedString() : UTF8String;
  var
    ks : UTF8String;
  begin
    if (line[linePos] <> '''') then
      raise Exception.CreateFmt('Unexpected character found "%s", a quote expected: "%s".',[line[linePos],CurrentLine()]);
    Inc(linePos);
    if (linePos > lineLength) then
      raise Exception.CreateFmt('Unexpected end of line, a quote expected: "%s".',[CurrentLine()]);
    if (line[linePos] = '''') then begin
      Inc(linePos);
      Result := '''';
      exit;
    end;
    Result := '';
    while (linePos <= lineLength) and ReadChar(ks) do begin
      Result := Result + ks;
      if (line[linePos] = '''') then
        break;
    end;
    if (line[linePos] = '''') then begin
      Inc(linePos);
      exit;
    end;
    raise Exception.CreateFmt('Unexpected end of line, a quote expected: "%s".',[line]);
  end;

  function ReadUnQuotedString() : UTF8String;
  var
    k : Integer;
  begin
    k := linePos;
    while (linePos <= lineLength) and
          not(CharInSet(line[linePos],[' ',#9,'#', '=','&','[',']','<','>','''','/','|']))
    do begin
      Inc(linePos);
    end;
    if (linePos > k) then begin
      if (line[linePos] in [' ',#9,'#', '=','&','[',']','<','>','''','/','|']) then
        Result := Copy(line,k,(linePos-k))
      else
        Result := Copy(line,k,(linePos-k)); //Result := Copy(line,k,(linePos-k+1));
    end else begin
      Result := '';
    end;
  end;

  function NextToken() : UTF8String; overload;
  var
    k : Integer;
    ks : UTF8String;
  begin
    specialChararter := False;
    while True do begin
      while (linePos <= lineLength) and CharInSet(line[linePos],[' ', #9, #13]) do begin
        Inc(linePos);
      end;
      if (linePos > lineLength) or (line[linePos] = '#') then begin
        if not NextLine() then
          exit('');
        Continue;
      end ;
      Break;
    end;
    if (linePos > lineLength) then
      exit('');

    if (line[linePos] = '*') then begin
      linePos := linePos+1;
      specialChararter := True;
      exit('*');
    end;
    k := linePos;
    if (linePos <= lineLength) and CharInSet(line[linePos],['<','>']) then begin
      ks := line[linePos];
      while (linePos <= lineLength) and (line[linePos] = ks) do begin
        Inc(linePos);
      end;
      Result := Copy(line,k,(linePos-k));
      exit;
    end;
    if (linePos <= lineLength) and
       CharInSet(line[linePos],['=','&','[',']','<','>','/','|'])
    then begin
      Inc(linePos);
      Result := Copy(line,k,(linePos-k));
      specialChararter := True;
      exit;
    end;
    {if (line[linePos] = '''') then
      exit(ReadQuotedString()); }
    Result := '';
    while (linePos <= lineLength) do begin
      if CharInSet(line[linePos],[' ',#9,#13,'#', '=','&','[',']','<','>','/','|']) then
        Break;
      if (line[linePos] <> '''') then
        ks := ReadUnQuotedString()
      else
        ks := ReadQuotedString();
      if (ks = '') then
        Break;
      Result := Result + ks;
    end;
  end;

  function NextToken(const AMustSucceed : Boolean) : UTF8String; overload;
  begin
    Result := NextToken();
    if (Result = '') and AMustSucceed then
      raise Exception.CreateFmt('Unexpected end of line(%d) : "%s".',[lineIndex,CurrentLine()]);
  end;

  procedure CheckToken(const AActual, AExpectedToken : UTF8String);
  begin
    if (AActual <> AExpectedToken) then
      raise Exception.CreateFmt(
              '"%s" expected but "%s" found at position %d, BufferPosition(%d), line(%d) = "%s".',
              [AExpectedToken,AActual,linePos,bufferPos,lineIndex,CurrentLine()]
            );
  end;

  function parse_reset() : Boolean;
  var
    s, s1 : UTF8String;
    logicalPos : TReorderLogicalReset;
    k : Integer;
  begin
    s := NextToken();
    if (s = '') then
      exit(False);
    CheckToken(s,'&');
    s := NextToken(True);
    if (s = '[') then begin
      s := NextToken();
      if (s = s_BEFORE) then begin
        s := NextToken();
        if not(TryStrToInt(s,k)) or (k < 1) or (k > 3) then
          CheckToken(s,'"1" or "2" or "3"');
        CheckToken(NextToken(True),']');
        statement^.Reset := String2UnicodeCodePointArray(NextToken(True));
        statement^.Before := True;
      end else begin
        while True do begin
          s1 := NextToken();
          if (s1 = '') or (s1 = ']') then
            break;
          s := s + Trim(s1)
        end;
        CheckToken(s1,']');
        if (s = '') then
          raise Exception.CreateFmt('Unexpected end of line : "%s".',[CurrentLine()]);
        if not TryStrToLogicalReorder(s,logicalPos) then
          raise Exception.CreateFmt(sUnknownResetLogicalPosition,[s]);
        statement^.LogicalPosition := logicalPos;
      end;
    end else begin
      statement^.Reset := String2UnicodeCodePointArray(s);
    end;
    if (statement^.LogicalPosition = TReorderLogicalReset.None) and
      (Length(statement^.Reset) = 0)
    then
      raise Exception.Create(sInvalidResetClause);
    Result := True;
  end;

  procedure EnsureElementLength(const ALength : Integer);
  var
    k, d : Integer;
  begin
    k := Length(statement^.Elements);
    if (k < ALength) then begin
      k := ALength;
      if (k = 0) then begin
        k := 50;
      end else begin
        if (k < 10) then
          d := 10
        else
          d := 2;
        k := k * d;
      end;
      statement^.SetElementCount(k);
    end;
  end;

  procedure AddElement(
    const AChars      : array of UCS4Char;
    const AWeigthKind : TReorderWeigthKind;
    const AContext    : UTF8String
  );overload;
  var
    kp : PReorderUnit;
    kc, k : Integer;
  begin
    EnsureElementLength(elementActualCount+1);
    kp := @statement^.Elements[elementActualCount];
    kc := Length(AChars)-1;
    if (kc < 0) then
      kc := 0;
    SetLength(kp^.Characters,kc);
    for k := 0 to kc - 1 do
     kp^.Characters[k] := AChars[k];
    kp^.WeigthKind := AWeigthKind;
    elementActualCount := elementActualCount + 1;
    if (AContext <> '') then
      kp^.Context := String2UnicodeCodePointArray(AContext);
  end;

  procedure AddElement(
    const AChar       : UCS4Char;
    const AWeigthKind : TReorderWeigthKind;
    const AContext    : UTF8String
  );overload;
  var
    kp : PReorderUnit;
    kc, k : Integer;
  begin
    EnsureElementLength(elementActualCount+1);
    kp := @statement^.Elements[elementActualCount];
    SetLength(kp^.Characters,1);
    kp^.Characters[0] := AChar;
    kp^.WeigthKind := AWeigthKind;
    elementActualCount := elementActualCount + 1;
    if (AContext <> '') then
      kp^.Context := String2UnicodeCodePointArray(AContext);
  end;

  function ReadNextItem() : Boolean;
  var
    contextStr : UTF8String;
    w : TReorderWeigthKind;
    last : PReorderUnit;
    u4str : UCS4String;
    s, ts : UTF8String;
    expandStr : TUnicodeCodePointArray;
    k, kc, x : Integer;
    us : UnicodeString;
  begin
    contextStr := '';
    expandStr := nil;
    Result := False;
    SaveState();
    s := NextToken();
    if (s = '') then begin
      DiscardState();
      exit;
    end;
    if specialChararter and (s = '&') then begin
      RestoreState();
      exit;
    end;
    DiscardState();
    if not TryStringToReorderWeigthKind(s,w) then
      CheckToken(s,'Reorder Weigth');
    s := NextToken(True);
    if specialChararter then begin
      if (s = '[') then begin
        k := 1;
        while True do begin
          ts := NextToken(True);
          s := s + ts;
          if specialChararter then begin
            if (ts = '[') then
              k := k+1
            else if (ts = ']') then begin
              k := k-1;
              if (k = 0) then
                Break;
            end;
          end;
        end;
        if (Pos('variable',s) > 0) then
          exit(True);
      end else if (s = '*') then begin
        s := NextToken(True);
        us := UTF8Decode(s);
        u4str := UnicodeStringToUCS4String(us);
        kc := Length(u4str)-1;
        k := 0;
        while (k <= (kc-1)) do begin
          if (k > 0) and (u4str[k] = Ord('-')) then begin
            if (k = (kc-1)) then begin
              AddElement(u4str[k],w,contextStr);
            end else begin
              for x := (u4str[k-1]+1) to u4str[k+1] do
                AddElement(x,w,contextStr);
              k := k+1;
            end;
          end else begin
            AddElement(u4str[k],w,contextStr);
          end;
          k := k+1;
        end;
        exit(True);
      end;
    end;
    SaveState();
    ts := NextToken();
    if (ts = '') or not(specialChararter) then begin
      RestoreState();
      us := UTF8Decode(s);
      u4str := UnicodeStringToUCS4String(us);
    end else begin
      if (ts = '|') then begin
        DiscardState();
        contextStr := s;
        s := NextToken(True);
        SaveState();
        ts := NextToken();
      end;
      if specialChararter and (ts = '/') then begin
        expandStr := String2UnicodeCodePointArray(NextToken(True));
        DiscardState();
      end else begin
        RestoreState();
      end;
      u4str := UnicodeStringToUCS4String(UTF8Decode(s));
    end;
    AddElement(u4str,w,contextStr);
    if (Length(expandStr) > 0) then begin
      last := @statement^.Elements[elementActualCount-1];
      last^.ExpansionChars := expandStr;
    end;
    Result := True;
  end;

begin
  Result := False;
  elementActualCount := 0;
  if (AStartPosition >= AMaxLen) then
    exit;
  historyItemIndex := -1;
  lineIndex := ALineCount;
  bufferLength := AMaxLen;
  bufferPos := AStartPosition;
  p := AData+AStartPosition;
  SetLength(line,LINE_LENGTH);
  statement := AStatement;
  statement^.Clear();
  if not NextLine() then
    exit;
  if not parse_reset() then
    exit;
  while ReadNextItem() do begin
    // All done in the condition
  end;
  statement^.SetElementCount(elementActualCount);
  if (linePos > lineLength) then
    linePos := lineLength;
  ANextPos := bufferPos-lineLength+linePos;
  Result := (ANextPos > AStartPosition);
  ALineCount := lineIndex;
end;

procedure ParseInitialDocument(ASequence : POrderedCharacters; ADoc : TCustomMemoryStream);
var
  buffer : PAnsiChar;
  bufferLength : Integer;
  i, nextPost : Integer;
  statement : TReorderSequence;
  p : PReorderUnit;
  lineCount : Integer;
begin
  if (ADoc.Size < 1) then
    exit;
  buffer := ADoc.Memory; //0xEF,0xBB,0xBF
  bufferLength := ADoc.Size;
  if (bufferLength >= 3) and
     (Byte(buffer[0]) = $EF) and
     (Byte(buffer[1]) = $BB) and
     (Byte(buffer[2]) = $BF)
  then begin
    Inc(buffer,3);
    Dec(bufferLength,3);
  end;
  lineCount := 0;
  ASequence^.Clear();
  SetLength(ASequence^.Data,50000);
  nextPost := 0;
  i := 0;
  while (i < bufferLength) do begin
    statement.Clear();
    if not ParseStatement(buffer,i,bufferLength,@statement,nextPost,lineCount) then
      Break;
    i := nextPost;
    try
      ASequence^.ApplyStatement(@statement);
    except
      on e : Exception do begin
        e.Message := Format('%s  Position = %d',[e.Message,i]);
        raise;
      end;
    end;
  end;
  if (ASequence^.ActualLength > 0) then begin
    p := @ASequence^.Data[0];
    for i := 0 to ASequence^.ActualLength - 1 do begin
      p^.Changed := False;
      Inc(p);
    end;
  end;
end;

procedure ParseInitialDocument(ASequence : POrderedCharacters; AFileName : string);
var
  doc : TMemoryStream;
begin
  doc := TMemoryStream.Create();
  try
    doc.LoadFromFile(AFileName);
    doc.Position := 0;
    ParseInitialDocument(ASequence,doc);
  finally
    doc.Free();
  end;
end;


end.

