const
  SErrInvalidPropertyType = 'Porco dio! %d';

type
  CharToOrdFuncty = Function(var charpo: Pointer): Cardinal;
  
function ReadWord(str : TStream) : word;
begin
  Result:=str.ReadWord;
  {$IFDEF ENDIAN_BIG}
  Result:=SwapEndian(Result);
  {$ENDIF}
end;

function ReadDWord(str : TStream) : longword;
begin
  Result:=str.ReadDWord;
  {$IFDEF ENDIAN_BIG}
  Result:=SwapEndian(Result);
  {$ENDIF}
end;


function CharToOrd(var P: Pointer): Cardinal;
begin
  result:= ord(pchar(P)^);
  inc(pchar(P));
end;

function WideCharToOrd(var P: Pointer): Cardinal;
begin
  result:= ord(pwidechar(P)^);
  inc(pwidechar(P));
end;

function Utf8ToOrd(var P:Pointer): Cardinal;
begin
  // Should also check for illegal utf8 combinations
  Result := Ord(PChar(P)^);
  Inc(P);
  if (Result and $80) <> 0 then
    if (Ord(Result) and %11100000) = %11000000 then begin
      Result := ((Result and %00011111) shl 6)
                or (ord(PChar(P)^) and %00111111);
      Inc(P);
    end else if (Ord(Result) and %11110000) = %11100000 then begin
      Result := ((Result and %00011111) shl 12)
                or ((ord(PChar(P)^) and %00111111) shl 6)
                or (ord((PChar(P)+1)^) and %00111111);
      Inc(P,2);
    end else begin
      Result := ((ord(Result) and %00011111) shl 18)
                or ((ord(PChar(P)^) and %00111111) shl 12)
                or ((ord((PChar(P)+1)^) and %00111111) shl 6)
                or (ord((PChar(P)+2)^) and %00111111);
      Inc(P,3);
    end;
end;


procedure MyObjectBinaryToText(Input, Output: TStream);

  procedure OutStr(s: String);
  begin
    if Length(s) > 0 then
      Output.Write(s[1], Length(s));
  end;

  procedure OutLn(s: String);
  begin
    OutStr(s + #10);
  end;

  procedure Outchars(P, LastP : Pointer; CharToOrdFunc: CharToOrdFuncty);

  var
    res, NewStr: String;
    w: Cardinal;
    InString, NewInString: Boolean;
  begin
   if p = nil then begin
    res:= '''''';
   end
   else
    begin
    res := '';
    InString := False;
    while P < LastP do
      begin
      NewInString := InString;
      w := CharToOrdfunc(P);
      if w = ord('''') then
        begin //quote char
        if not InString then
          NewInString := True;
        NewStr := '''''';
        end
      else if (Ord(w) >= 32) and (Ord(w) < 127) then
        begin //printable ascii
        if not InString then
          NewInString := True;
        NewStr := char(w);
        end
      else
        begin //ascii control chars, non ascii
        if InString then
          NewInString := False;
        NewStr := '#' + IntToStr(w);
        end;
      if NewInString <> InString then
        begin
        NewStr := '''' + NewStr;
        InString := NewInString;
        end;
      res := res + NewStr;
      end;
    if InString then
      res := res + '''';
    end;
   OutStr(res);
  end;

  procedure OutString(s: String);

  begin
    OutChars(Pointer(S),PChar(S)+Length(S),@CharToOrd);
  end;

  procedure OutWString(W: WideString);

  begin
    OutChars(Pointer(W),pwidechar(W)+Length(W),@WideCharToOrd);
  end;

  procedure OutUtf8Str(s: String);
  begin
    OutChars(Pointer(S),PChar(S)+Length(S),@Utf8ToOrd);
  end;

  function ReadInt(ValueType: TValueType): LongInt;
  begin
    case ValueType of
      vaInt8: Result := ShortInt(Input.ReadByte);
      vaInt16: Result := SmallInt(ReadWord(Input));
      vaInt32: Result := LongInt(ReadDWord(Input));
    end;
  end;

  function ReadInt: LongInt;
  begin
    Result := ReadInt(TValueType(Input.ReadByte));
  end;

  function ReadSStr: String;
  var
    len: Byte;
  begin
    len := Input.ReadByte;
    SetLength(Result, len);
    if (len > 0) then
      Input.Read(Result[1], len);
  end;

  function ReadLStr: String;
  var
    len: DWord;
  begin
    len := ReadDWord(Input);
    SetLength(Result, len);
    if (len > 0) then
      Input.Read(Result[1], len);
  end;

  function ReadWStr: WideString;
  var
    len: DWord;
  begin
    len := ReadDword(Input);
    SetLength(Result, len);
    if (len > 0) then
      Input.Read(Pointer(@Result[1])^, len*2);
  end;

  procedure ReadPropList(indent: String);

    procedure ProcessValue(ValueType: TValueType; Indent: String);

      procedure ProcessBinary;
      var
        ToDo, DoNow, i: LongInt;
        lbuf: array[0..31] of Byte;
        s: String;
      begin
        ToDo := ReadDWord(Input);
        OutLn('{');
        while ToDo > 0 do begin
          DoNow := ToDo;
          if DoNow > 32 then DoNow := 32;
          Dec(ToDo, DoNow);
          s := Indent + '  ';
          Input.Read(lbuf, DoNow);
          for i := 0 to DoNow - 1 do
            s := s + IntToHex(lbuf[i], 2);
          OutLn(s);
        end;
        OutLn(indent + '}');
      end;

    var
      s: String;
{      len: LongInt; }
      IsFirst: Boolean;
      ext: Extended;

    begin
      case ValueType of
        vaList: begin
            OutStr('(');
            IsFirst := True;
            while True do begin
              ValueType := TValueType(Input.ReadByte);
              if ValueType = vaNull then break;
              if IsFirst then begin
                OutLn('');
                IsFirst := False;
              end;
              OutStr(Indent + '  ');
              ProcessValue(ValueType, Indent + '  ');
            end;
            OutLn(Indent + ')');
          end;
        vaInt8: OutLn(IntToStr(ShortInt(Input.ReadByte)));
        vaInt16: OutLn( IntToStr(SmallInt(ReadWord(Input))));
        vaInt32: OutLn(IntToStr(LongInt(ReadDWord(Input))));
        vaExtended: begin
            Input.Read(ext, SizeOf(ext));
            Str(ext,S);// Do not use localized strings.
            OutLn(S);
          end;
        vaString: begin
            OutString(ReadSStr);
            OutLn('');
          end;
        vaIdent: OutLn(ReadSStr);
        vaFalse: OutLn('False');
        vaTrue: OutLn('True');
        vaBinary: ProcessBinary;
        vaSet: begin
            OutStr('[');
            IsFirst := True;
            while True do begin
              s := ReadSStr;
              if Length(s) = 0 then break;
              if not IsFirst then OutStr(', ');
              IsFirst := False;
              OutStr(s);
            end;
            OutLn(']');
          end;
        vaLString:
          begin
          OutString(ReadLStr);
          OutLn('');
          end;
        vaWString:
          begin
          OutWString(ReadWStr);
          OutLn('');
          end;
        vaNil:
          OutLn('nil');
        vaCollection: begin
            OutStr('<');
            while Input.ReadByte <> 0 do begin
              OutLn(Indent);
              Input.Seek(-1, soFromCurrent);
              OutStr(indent + '  item');
              ValueType := TValueType(Input.ReadByte);
              if ValueType <> vaList then
                OutStr('[' + IntToStr(ReadInt(ValueType)) + ']');
              OutLn('');
              ReadPropList(indent + '    ');
              OutStr(indent + '  end');
            end;
            OutLn('>');
          end;
        {vaSingle: begin OutLn('!!Single!!'); exit end;
        vaCurrency: begin OutLn('!!Currency!!'); exit end;
        vaDate: begin OutLn('!!Date!!'); exit end;}
        vaUTF8String: begin
            OutUtf8Str(ReadLStr);
            OutLn('');
          end;
        else
          Raise EReadError.CreateFmt(SErrInvalidPropertyType,[Ord(ValueType)]);
      end;
    end;

  var mystr : shortstring;
  begin
    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      mystr:=ReadSStr;
      OutStr(indent + mystr + ' = ');
      ProcessValue(TValueType(Input.ReadByte), Indent);
    end;
  end;

  procedure ReadObject(indent: String);
  var
    b: Byte;
    ObjClassName, ObjName: String;
    ChildPos: LongInt;
  begin
    // Check for FilerFlags
    b := Input.ReadByte;
    if (b and $f0) = $f0 then begin
      if (b and 2) <> 0 then ChildPos := ReadInt;
    end else begin
      b := 0;
      Input.Seek(-1, soFromCurrent);
    end;

    ObjClassName := ReadSStr;
    ObjName := ReadSStr;

    OutStr(Indent);
    if (b and 1) <> 0 then OutStr('inherited')
    else
     if (b and 4) <> 0 then OutStr('inline')
     else OutStr('object');
    OutStr(' ');
    if ObjName <> '' then
      OutStr(ObjName + ': ');
    OutStr(ObjClassName);
    if (b and 2) <> 0 then OutStr('[' + IntToStr(ChildPos) + ']');
    OutLn('');

    ReadPropList(indent + '  ');

    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      ReadObject(indent + '  ');
    end;
    OutLn(indent + 'end');
  end;

type
  PLongWord = ^LongWord;
const
  signature: PChar = 'TPF0';

begin
  if Input.ReadDWord <> PLongWord(Pointer(signature))^ then
    raise EReadError.Create('Illegal stream image' {###SInvalidImage});
  ReadObject('');
end;

