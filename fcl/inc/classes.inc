{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{**********************************************************************
 *       Class implementations are in separate files.                 *
 **********************************************************************}

{
 Include all message strings

 Add a language with IFDEF LANG_NAME
 just befor the final ELSE. This way English will always be the default.
}

{$IFDEF LANG_GERMAN}
{$i constsg.inc}
{$ELSE}
{$IFDEF LANG_SPANISH}
{$i constss.inc}
{$ELSE}
{$i constse.inc}
{$ENDIF}
{$ENDIF}

{ Utility routines }
{$i util.inc}

{ TBits implementation }
{$i bits.inc}

{ TReader implementation }
{ $i reader.inc}

{ TWriter implementations }
{$i writer.inc}
{$i twriter.inc}

{ TFiler implementation }
{$i filer.inc}

{ All streams implementations: }
{ Tstreams THandleStream TFileStream TResourcseStreams TStringStream }
{ TCustomMemoryStream TMemoryStream }
{$i streams.inc}

{ TParser implementation}
{$i parser.inc}

{ TCollection and TCollectionItem implementations }
{$i collect.inc}

{ TList and TThreadList implementations }
{$i lists.inc}

{ TStrings and TStringList implementations }
{$i stringl.inc}

{ TThread implementation }
{$i thread.inc}

{ TPersistent implementation }
{$i persist.inc }

{ TComponent implementation }
{$i compon.inc}

{ Class and component registration routines }
{$I cregist.inc}


{**********************************************************************
 *       Miscellaneous procedures and functions                       *
 **********************************************************************}

{ Point and rectangle constructors }

function Point(AX, AY: Integer): TPoint;

begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;


function SmallPoint(AX, AY: SmallInt): TSmallPoint;

begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;


function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;

begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;


function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;

begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ALeft + AWidth;
    Bottom :=  ATop + AHeight;
  end;
end;





{ Object filing routines }

procedure RegisterIntegerConsts(IntegerType: Pointer; IdentToInt: TIdentToInt;
  IntToIdent: TIntToIdent);

begin
end;


function IdentToInt(const Ident: string; var Int: Longint; const Map: array of TIdentMapEntry): Boolean;

begin
  IdentToInt:=false;
end;


function IntToIdent(Int: Longint; var Ident: string; const Map: array of TIdentMapEntry): Boolean;

begin
  IntToIdent:=false;
end;


function InitInheritedComponent(Instance: TComponent; RootAncestor: TClass): Boolean;

begin
  InitInheritedComponent:=false;
end;


function InitComponentRes(const ResName: string; Instance: TComponent): Boolean;

begin
  InitComponentRes:=false;
end;


function ReadComponentRes(const ResName: string; Instance: TComponent): TComponent;

begin
  ReadComponentRes:=nil;
end;


function ReadComponentResEx(HInstance: THandle; const ResName: string): TComponent;

begin
  ReadComponentResEx:=nil;
end;


function ReadComponentResFile(const FileName: string; Instance: TComponent): TComponent;

begin
  ReadComponentResFile:=nil;
end;


procedure WriteComponentResFile(const FileName: string; Instance: TComponent);

begin
end;



procedure GlobalFixupReferences;

begin
end;


procedure GetFixupReferenceNames(Root: TComponent; Names: TStrings);

begin
end;


procedure GetFixupInstanceNames(Root: TComponent;
  const ReferenceRootName: string; Names: TStrings);

begin
end;


procedure RedirectFixupReferences(Root: TComponent; const OldRootName,
  NewRootName: string);

begin
end;


procedure RemoveFixupReferences(Root: TComponent; const RootName: string);

begin
end;


procedure RemoveFixups(Instance: TPersistent);

begin
end;



procedure BeginGlobalLoading;

begin
end;


procedure NotifyGlobalLoading;

begin
end;


procedure EndGlobalLoading;

begin
end;



function CollectionsEqual(C1, C2: TCollection): Boolean;

begin
  CollectionsEqual:=false;
end;



{ Object conversion routines }

procedure ObjectBinaryToText(Input, Output: TStream);

  procedure OutStr(s: String);
  begin
    if Length(s) > 0 then
      Output.Write(s[1], Length(s));
  end;

  procedure OutLn(s: String);
  begin
    OutStr(s + #10);
  end;

  procedure OutString(s: String);
  var
    res, NewStr: String;
    i: Integer;
    InString, NewInString: Boolean;
  begin
    res := '';
    InString := False;
    for i := 1 to Length(s) do begin
      NewInString := InString;
      case s[i] of
        #0..#31: begin
            if InString then
              NewInString := False;
            NewStr := '#' + IntToStr(Ord(s[i]));
          end;
        '''':
            if InString then NewStr := ''''''
            else NewStr := '''''''';
        else begin
          if not InString then
            NewInString := True;
          NewStr := s[i];
        end;
      end;
      if NewInString <> InString then begin
        NewStr := '''' + NewStr;
        InString := NewInString;
      end;
      res := res + NewStr;
    end;
    if InString then res := res + '''';
    OutStr(res);
  end;

  function ReadInt(ValueType: TValueType): LongInt;
  begin
    case ValueType of
      vaInt8: Result := ShortInt(Input.ReadByte);
      vaInt16: Result := SmallInt(Input.ReadWord);
      vaInt32: Result := LongInt(Input.ReadDWord);
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
    Input.Read(Result[1], len);
  end;

  procedure ReadPropList(indent: String);

    procedure ProcessValue(ValueType: TValueType; Indent: String);

      procedure Stop(s: String);
      begin
        WriteLn(s);
        Halt;
      end;

      procedure ProcessBinary;
      var
        ToDo, DoNow, i: LongInt;
        lbuf: array[0..31] of Byte;
        s: String;
      begin
        ToDo := Input.ReadDWord;
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
      len: LongInt;
      IsFirst: Boolean;
      ext: Extended;

    begin
      OutStr('(' + IntToStr(Ord(Valuetype)) + ') ');
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
        vaInt16: OutLn( IntToStr(SmallInt(Input.ReadWord)));
        vaInt32: OutLn(IntToStr(LongInt(Input.ReadDWord)));
        vaExtended: begin
            Input.Read(ext, SizeOf(ext));
            OutLn(FloatToStr(ext));
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
        vaLString: Stop('!!LString!!');
        vaNil: Stop('nil');
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
        vaDate: begin OutLn('!!Date!!'); exit end;
        vaWString: begin OutLn('!!WString!!'); exit end;}
        else
          Stop(IntToStr(Ord(ValueType)));
      end;
    end;

  begin
    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      OutStr(indent + ReadSStr + ' = ');
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


procedure ObjectTextToBinary(Input, Output: TStream);
var
  parser: TParser;

  procedure WriteString(s: String);
  begin
    Output.WriteByte(Length(s));
    Output.Write(s[1], Length(s));
  end;

  procedure WriteInteger(value: LongInt);
  begin
    if (value >= -128) and (value <= 127) then begin
      Output.WriteByte(Ord(vaInt8));
      Output.WriteByte(Byte(value));
    end else if (value >= -32768) and (value <= 32767) then begin
      Output.WriteByte(Ord(vaInt16));
      Output.WriteWord(Word(value));
    end else begin
      Output.WriteByte(ord(vaInt32));
      Output.WriteDWord(LongWord(value));
    end;
  end;

  procedure ProcessProperty; forward;

  procedure ProcessValue;
  var
    flt: Extended;
    s: String;
    stream: TMemoryStream;
  begin
    case parser.Token of
      toInteger: WriteInteger(parser.TokenInt);
      toFloat: begin
          Output.WriteByte(Ord(vaExtended));
          flt := Parser.TokenFloat;
          Output.Write(flt, SizeOf(flt));
        end;
      toString: begin
          s := parser.TokenString;
          while parser.NextToken = '+' do begin
            parser.NextToken;   // Get next string fragment
            parser.CheckToken(toString);
            s := s + parser.TokenString;
          end;
          Output.WriteByte(Ord(vaString));
          WriteString(s);
        end;
      toSymbol:
          if CompareText(parser.TokenString, 'True') = 0 then
            Output.WriteByte(Ord(vaTrue))
          else if CompareText(parser.TokenString, 'False') = 0 then
            Output.WriteByte(Ord(vaFalse))
          else if CompareText(parser.TokenString, 'nil') = 0 then
            Output.WriteByte(Ord(vaNil))
          else begin
            Output.WriteByte(Ord(vaIdent));
            WriteString(parser.TokenString);
          end;
      // Set
      '[': begin
          parser.NextToken;
          Output.WriteByte(Ord(vaSet));
          if parser.Token <> ']' then
            while True do begin
              parser.CheckToken(toSymbol);
              WriteString(parser.TokenString);
              parser.NextToken;
              if parser.Token = ']' then break;
              parser.CheckToken(',');
              parser.NextToken;
            end;
          Output.WriteByte(0);
        end;
      // List
      '(': begin
          parser.NextToken;
          Output.WriteByte(Ord(vaList));
          while parser.Token <> ')' do ProcessValue;
          Output.WriteByte(0);
        end;
      // Collection
      '<': begin
          parser.NextToken;
          Output.WriteByte(Ord(vaCollection));
          while parser.Token <> '>' do begin
            parser.CheckTokenSymbol('item');
            parser.NextToken;
            // ConvertOrder
            Output.WriteByte(Ord(vaList));
            while not parser.TokenSymbolIs('end') do ProcessProperty;
            parser.NextToken;   // Skip 'end'
            Output.WriteByte(0);
          end;
          Output.WriteByte(0);
        end;
      // Binary data
      '{': begin
          Output.WriteByte(Ord(vaBinary));
          stream := TMemoryStream.Create;
          try
            parser.HexToBinary(stream);
            Output.WriteDWord(stream.Size);
            Output.Write(Stream.Memory^, stream.Size);
          finally
            stream.Free;
          end;
        end;
    else WriteLn('Token: "', parser.Token, '" ', Ord(parser.Token));
    end;
    parser.NextToken;
  end;

  procedure ProcessProperty;
  var
    name: String;
  begin
    // Get name of property
    parser.CheckToken(toSymbol);
    name := parser.TokenString;
    while True do begin
      parser.NextToken;
      if parser.Token <> '.' then break;
      parser.NextToken;
      parser.CheckToken(toSymbol);
      name := name + '.' + parser.TokenString;
    end;
    // WriteLn(name);
    WriteString(name);
    parser.CheckToken('=');
    parser.NextToken;
    ProcessValue;
  end;

  procedure ProcessObject;
  var
    IsInherited: Boolean;
    ObjectName, ObjectType: String;
  begin
    if parser.TokenSymbolIs('OBJECT') then
      IsInherited := False
    else begin
      parser.CheckTokenSymbol('INHERITED');
      IsInherited := True;
    end;
    parser.NextToken;
    parser.CheckToken(toSymbol);
    ObjectName := '';
    ObjectType := parser.TokenString;
    parser.NextToken;
    if parser.Token = ':' then begin
      parser.NextToken;
      parser.CheckToken(toSymbol);
      ObjectName := ObjectType;
      ObjectType := parser.TokenString;
      parser.NextToken;
    end;
    WriteString(ObjectType);
    WriteString(ObjectName);

    // Convert property list
    while not (parser.TokenSymbolIs('END') or
      parser.TokenSymbolIs('OBJECT') or
      parser.TokenSymbolIs('INHERITED')) do
      ProcessProperty;
    Output.WriteByte(0);        // Terminate property list

    // Convert child objects
    while not parser.TokenSymbolIs('END') do ProcessObject;
    parser.NextToken;           // Skip end token
    Output.WriteByte(0);        // Terminate property list
  end;

const
  signature: PChar = 'TPF0';
begin
  parser := TParser.Create(Input);
  try
    Output.Write(signature[0], 4);
    ProcessObject;
  finally
    parser.Free;
  end;
end;


procedure ObjectResourceToText(Input, Output: TStream);
begin
  Input.ReadResHeader;
  ObjectBinaryToText(Input, Output);
end;


procedure ObjectTextToResource(Input, Output: TStream);
var
  StartPos, SizeStartPos, BinSize: LongInt;
  parser: TParser;
  name: String;
begin
  // Get form type name
  StartPos := Input.Position;
  parser := TParser.Create(Input);
  try
    if not parser.TokenSymbolIs('OBJECT') then parser.CheckTokenSymbol('INHERITED');
    parser.NextToken;
    parser.CheckToken(toSymbol);
    parser.NextToken;
    parser.CheckToken(':');
    parser.NextToken;
    parser.CheckToken(toSymbol);
    name := parser.TokenString;
  finally
    parser.Free;
    Input.Position := StartPos;
  end;

  // Write resource header
  name := UpperCase(name);
  Output.WriteByte($ff);
  Output.WriteByte(10);
  Output.WriteByte(0);
  Output.Write(name[1], Length(name) + 1);      // Write null-terminated form type name
  Output.WriteWord($1030);
  SizeStartPos := Output.Position;
  Output.WriteDWord(0);                 // Placeholder for data size
  ObjectTextToBinary(Input, Output);    // Convert the stuff!
  BinSize := Output.Position - SizeStartPos - 4;
  Output.Position := SizeStartPos;
  Output.WriteDWord(BinSize);           // Insert real resource data size
end;



{ Utility routines }

function LineStart(Buffer, BufPos: PChar): PChar;

begin
  Result := BufPos;
  while Result > Buffer do begin
    Dec(Result);
    if Result[0] = #10 then break;
  end;
end;




{
  $Log$
  Revision 1.16  2000-01-07 01:24:33  peter
    * updated copyright to 2000

  Revision 1.15  2000/01/06 01:20:32  peter
    * moved out of packages/ back to topdir

  Revision 1.2  2000/01/04 18:07:16  michael
  + Streaming implemented

  Revision 1.1  2000/01/03 19:33:06  peter
    * moved to packages dir

  Revision 1.13  1999/10/19 11:27:03  sg
  * Added DFM<->ASCII conversion procedures

  Revision 1.12  1999/09/30 19:31:42  fcl
  * Implemented LineStart  (sg)

  Revision 1.11  1999/09/11 21:59:31  fcl
  * Moved class and registration functions to cregist.inc  (sg)

  Revision 1.10  1999/04/13 08:52:29  michael
  + Moved strings.inc to stringl.inc, to avoid conflict with strings unit

  Revision 1.9  1999/04/08 10:18:50  peter
    * makefile updates

}
