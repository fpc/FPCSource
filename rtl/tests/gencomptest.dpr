program gencomptest;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  classes,
  typinfo,
  tcstreaming in 'tcstreaming.pas',
  testcomps in 'testcomps.pas';

Var
  Indent : String;
  Src,
  Procs : TStrings;

Procedure AddLn(S : String); overload;

begin
  Src.Add(Indent+S);
end;

Procedure AddLn(Fmt : String; Args : Array of Const); overload;

begin
  AddLn(Format(Fmt,Args));
end;

Function CreateString(S : String) : string;

begin
  Result:=StringReplace(S,'''','''''',[rfReplaceAll]);
  Result:=''''+Result+'''';
end;

Function ValName(V : TValueType) : String;

begin
  Result:=GetEnumName(TypeInfo(TValueType),Ord(v));
end;

Function AddExpectValue(V : TValueType) : String;

begin
  AddLn('ExpectValue(%s);',[ValName(V)]);
end;


// This is a reworked version of ObjectBinaryToText.
// Instead of a text stream, it outputs testsuite code.
// Note it will only work on i386/AMD64 platforms.

Procedure AnalyzeStream(Input : TStream);

var
  NestingLevel: Integer;
  SaveSeparator: Char;
  Reader: TReader;
  ObjectName, PropName: string;


  procedure ConvertValue; forward;

  procedure ConvertHeader;
  var
    ClassName: string;
    Flags: TFilerFlags;
    F : TFilerFlag;
    Position: Integer;
    S : String;

  begin
    Position:=0;
    Reader.ReadPrefix(Flags, Position);
    S:='';
    For F:=Low(TFilerFlag) to High(TFilerFlag) do
      if F in Flags then
        begin
        If (S<>'') then
          S:=S+',';
        S:=S+GetEnumName(TypeInfo(TFilerFlag),Ord(F));
        end;
    Addln('ExpectFlags([%s],%d);',[S,Position]);
    ClassName := Reader.ReadStr;
    Addln('ExpectBareString(%s);',[CreateString(ClassName)]);
    ObjectName := Reader.ReadStr;
    Addln('ExpectBareString(%s);',[CreateString(ObjectName)]);
  end;

  procedure ConvertBinary;
  const
    BytesPerLine = 32;
  var
    I,j: Integer;
    Count: Longint;
    Buffer: array[0..BytesPerLine - 1] of Char;
    V : TValueTYpe;

  begin
    V:=Reader.ReadValue;
    AddExpectValue(V);
    Reader.Read(Count, SizeOf(Count));
    Addln('ExpectInteger(%d);',[Count]);
    while Count > 0 do
      begin
      if Count >= 32 then I := 32 else I := Count;
      Reader.Read(Buffer, I);
      For J:=0 to I-1 do
        Addln('ExpectByte(%d);',[Byte(Buffer[J])]);
      Dec(Count, I);
      end;
  end;

  procedure ConvertProperty; forward;

  procedure ConvertValue;
  var
    S: string;
    W: WideString;
    V : TValueType;

  begin
    V:=Reader.NextValue;
    case V of
      vaList:
        begin
          V:=Reader.ReadValue;
          AddExpectValue(V);
          Inc(NestingLevel);
          while not Reader.EndOfList do
          begin
            ConvertValue;
          end;
          Reader.ReadListEnd;
          Addln('ExpectListEnd');
          Dec(NestingLevel);
        end;
      vaInt8, vaInt16, vaInt32:
        begin
        Addln('ExpectInteger(%d);',[Reader.ReadInteger]);
        end;
      vaExtended:
        Addln('ExpectExtended(%f);',[Reader.ReadFloat]);
      vaSingle:
        Addln('ExpectSingle(%f);',[Reader.ReadSingle]);
      vaCurrency:
        Addln('ExpectCurrency(%f);',[Reader.ReadCurrency]);
      vaDate:
        Addln('ExpectDate(%f);',[Reader.ReadDate]);
      vaWString, vaUTF8String:
        begin
          W := Reader.ReadWideString;
          Addln('ExpectWideString(%s);',[CreateString(W)]);
        end;
      vaString, vaLString:
        begin
          S := Reader.ReadString;
          Addln('ExpectString(%s);',[CreateString(S)]);
        end;
      vaIdent, vaFalse, vaTrue, vaNil, vaNull:
        Addln('ExpectIdent(%s);',[CreateString(Reader.ReadIdent)]);
      vaBinary:
        ConvertBinary;
      vaSet:
        begin
          V:=Reader.ReadValue;
          AddExpectValue(V);
          while True do
            begin
            S := Reader.ReadStr;
            Addln('ExpectBareString(%s);',[CreateString(S)]);
            if S = '' then Break;
            end;
        end;
      vaCollection:
        begin
          V:=Reader.ReadValue;
          AddExpectValue(V);
          Inc(NestingLevel);
          while not Reader.EndOfList do
          begin
            V:=Reader.NextValue;
            if V in [vaInt8, vaInt16, vaInt32] then
              begin
              ConvertValue;
              end;
            Reader.CheckValue(vaList);
            AddExpectValue(vaList);
            Inc(NestingLevel);
            while not Reader.EndOfList do
              ConvertProperty;
            Reader.ReadListEnd;
            Addln('ExpectEndOfList;');
            Dec(NestingLevel);
          end;
          Reader.ReadListEnd;
          Addln('ExpectEndOfList;');
          Dec(NestingLevel);
        end;
      vaInt64:
        Addln('ExpectInt64(%d);',[Reader.ReadInt64]);
    else
       Raise Exception.Create('Invalid stream');
    end;
  end;

  procedure ConvertProperty;
  begin
    PropName := Reader.ReadStr;  // save for error reporting
    Addln('ExpectBareString(%s);',[CreateString(PropName)]);
    ConvertValue;
  end;

  procedure ConvertObject;
  begin
    ConvertHeader;
    Inc(NestingLevel);
    while not Reader.EndOfList do ConvertProperty;
    Reader.ReadListEnd;
    Addln('ExpectEndOfList;');
    while not Reader.EndOfList do ConvertObject;
    Reader.ReadListEnd;
    Addln('ExpectEndOfList;');
    Dec(NestingLevel);
  end;

begin
  NestingLevel := 0;
  Reader := TReader.Create(Input, 4096);
  SaveSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    Reader.ReadSignature;
    Addln('ExpectSignature;');
    ConvertObject;
  finally
    DecimalSeparator := SaveSeparator;
    Reader.Free;
  end;
end;

Procedure TestComponent(AClass : TComponentClass; AOwner : TComponent);

Var
  S : TMemoryStream;
  C : TComponent;
  N,O : String;

begin
  Addln('');
  Addln('');
  Addln('Procedure TTestComponentStream.Test%s;',[AClass.ClassName]);
  Addln('');
  Addln('Var');
  Addln('  C : TComponent;');
  Addln('');
  Addln('begin');
  Indent:='  ';
  N:=AClass.ClassName;
  Procs.Add('Test'+N);
  If (AOwner=Nil) then
    O:='Nil'
  else
    O:=AOwner.Name;
  AddLn('C:=%s.Create(%s);',[N,O]);
  Addln('Try');
  Indent:='    ';
  Addln('SaveToStream(C);');
  S:=TMemoryStream.Create;
  try
    C:=AClass.Create(AOwner);
    Try
      C.Name:='Test'+C.ClassName;
      S.WriteComponent(C);
      S.Position:=0;
      AnalyzeStream(S);
      With TFileStream.Create(AClass.ClassName+'.dat',fmCreate) do
        try
          CopyFrom(S,0);
        finally
          Free;
        end;
    Finally
      C.Free;
    end;
  finally
    S.Free;
  end;
  Indent:='  ';
  Addln('Finally');
  Indent:='    ';
  Addln('C.Free;');
  Addln('end;');
  Indent:='';
  Addln('end;');
end;

Procedure GenTests;

begin
  TestComponent(TEmptyComponent,Nil);
  TestComponent(TIntegerComponent,Nil);
  TestComponent(TIntegerComponent2,Nil);
  TestComponent(TIntegerComponent3,Nil);
  TestComponent(TIntegerComponent4,Nil);
  TestComponent(TIntegerComponent5,Nil);
  TestComponent(TInt64Component,Nil);
  TestComponent(TInt64Component2,Nil);
  TestComponent(TInt64Component3,Nil);
  TestComponent(TInt64Component4,Nil);
  TestComponent(TInt64Component5,Nil);
  TestComponent(TInt64Component6,Nil);
  TestComponent(TStringComponent,Nil);
  TestComponent(TStringComponent2,Nil);
  TestComponent(TWideStringComponent,Nil);
  TestComponent(TWideStringComponent2,Nil);
  TestComponent(TSingleComponent,Nil);
  TestComponent(TDoubleComponent,Nil);
  TestComponent(TExtendedComponent,Nil);
  TestComponent(TCompComponent,Nil);
  TestComponent(TCurrencyComponent,Nil);
  TestComponent(TDateTimeComponent,Nil);
  TestComponent(TDateTimeComponent2,Nil);
  TestComponent(TDateTimeComponent3,Nil);
  TestComponent(TEnumComponent,Nil);
  TestComponent(TEnumComponent2,Nil);
  TestComponent(TEnumComponent3,Nil);
  TestComponent(TEnumComponent4,Nil);
  TestComponent(TSetComponent,Nil);
  TestComponent(TSetComponent2,Nil);
  TestComponent(TSetComponent3,Nil);
  TestComponent(TSetComponent4,Nil);
  TestComponent(TMultipleComponent,Nil);
  TestComponent(TPersistentComponent,Nil);
  TestComponent(TCollectionComponent,Nil);
  TestComponent(TCollectionComponent2,Nil);
  TestComponent(TCollectionComponent3,Nil);
  TestComponent(TCollectionComponent4,Nil);
  TestComponent(TOwnedComponent,Nil);
  TestComponent(TStreamedOwnedComponent,Nil);
  TestComponent(TMethodComponent,Nil);
  TestComponent(TMethodComponent2,Nil);
end;


Procedure GenUnit;

Var
  I : Integer;
  F : Text;

begin
  Assign(f,'tctestcompstreaming.pas');
  Rewrite(F);
  try
  Writeln(F,'Unit tctestcompstreaming;');
  Writeln(F);
  Writeln(F,'interface');
  Writeln(F);
  Writeln(F,'Uses');
  Writeln(F,'  SysUtils,Classes,tcstreaming;');
  Writeln(F);
  Writeln(F,'Type ');
  Writeln(F,'  TTestComponentStream = Class(TTestStreaming)');
  Writeln(F,'  Published');
  For I:=0 to Procs.Count-1 do
    Writeln(F,'    Procedure '+Procs[i]+';');
  Writeln(F,'  end;');
  Writeln(F);
  Writeln(F,'Implementation');
  Writeln(F);
  Writeln(F,'uses testcomps;');
  For I:=0 to Src.Count-1 do
    Writeln(F,Src[i]);
  Writeln(F);
  Writeln(F,'end.');
  Finally
    Close(f);
  end;
end;

Procedure GenCode;

begin
  Src:=TStringList.Create;
  try
    Procs:=TStringList.Create;
    try
      GenTests;
      GenUnit;
    finally
      Procs.Free;
    end;
  finally
    Src.Free;
  end;
end;


begin
  GenCode;
end.
