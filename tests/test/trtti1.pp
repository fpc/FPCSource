Program trtti1;

{$Mode Delphi}
{$M+}

Uses
  Typinfo;

Const TypeNames : Array [TTYpeKind] of string[15] =
                    ('Unknown','Integer','Char','Enumeration',
                     'Float','Set','Method','ShortString','LongString',
                     'AnsiString','WideString','Variant','Array','Record',
                     'Interface','Class','Object','WideChar','Bool','Int64','QWord',
                     'DynamicArray','RawInterface');

Const OrdinalTypes = [tkInteger,tkChar,tkENumeration,tkbool];

Type
   TMyEnum = (meFirst,meSecond,meThird);
   TMyTestObject = Class(TObject)
       Private
       FBoolean  : Boolean;
       FByte     : Byte;
       FChar     : Char;
       FWord     : Word;
       FInteger  : Integer;
       Flongint  : Longint;
       FCardinal : Cardinal;
       FReal     : Real;
       FExtended : Extended;
       FMyEnum   : TMyEnum;
       FAnsiString   : AnsiSTring;
       FObj      : TObject;
       FIntf      : IInterface;
       FStored   : Boolean;
       Function GetBoolean : Boolean;
       Function GetByte : Byte;
       Function GetChar : Char;
       Function GetWord : Word;
       Function GetInteger : Integer;
       Function GetLongint : Longint;
       Function GetCardinal : Cardinal;
       Function GetReal : Real;
       Function GetExtended : Extended;
       Function GetAnsiString : AnsiString;
       Function GetMyEnum : TMyEnum;
       Procedure SetBoolean ( Value : Boolean);
       Procedure SetByte ( Value : Byte );
       Procedure SetChar ( Value : Char );
       Procedure SetWord ( Value : Word );
       Procedure SetInteger ( Value : Integer );
       Procedure SetLongint ( Value : Longint );
       Procedure SetCardinal ( Value : Cardinal );
       Procedure SetReal ( Value : Real );
       Procedure SetExtended ( Value : Extended );
       Procedure SetAnsiString ( Value : AnsiString );
       Procedure SetMyEnum ( Value : TMyEnum );
       Function GetVirtualBoolean : Boolean; virtual;
       Function GetVirtualByte : Byte; virtual;
       Function GetVirtualChar : Char; virtual;
       Function GetVirtualWord : Word; virtual;
       Function GetVirtualInteger : Integer; virtual;
       Function GetVirtualLongint : Longint; virtual;
       Function GetVirtualCardinal : Cardinal; virtual;
       Function GetVirtualReal : Real; virtual;
       Function GetVirtualExtended : Extended; virtual;
       Function GetVirtualAnsiString : AnsiString; virtual;
       Function GetVirtualMyEnum : TMyEnum; virtual;
       Procedure SetVirtualBoolean ( Value : Boolean); virtual;
       Procedure SetVirtualByte ( Value : Byte ); virtual;
       Procedure SetVirtualChar ( Value : Char ); virtual;
       Procedure SetVirtualWord ( Value : Word ); virtual;
       Procedure SetVirtualInteger ( Value : Integer ); virtual;
       Procedure SetVirtualLongint ( Value : Longint ); virtual;
       Procedure SetVirtualCardinal ( Value : Cardinal ); virtual;
       Procedure SetVirtualReal ( Value : Real ); virtual;
       Procedure SetVirtualExtended ( Value : Extended ); virtual;
       Procedure SetVirtualAnsiString ( Value : AnsiString ); virtual;
       Procedure SetVirtualMyEnum ( Value : TMyEnum ); virtual;
       Function GetStaticStored : Boolean;
       Function GetVirtualStored : Boolean;virtual;
       Public
       Constructor Create;
       Destructor Destroy;override;
       Published
       Property ObjField: TObject read FObj write FObj;
       Property IntfField: IInterface read FIntf write FIntf;
       Property BooleanField : Boolean Read FBoolean Write FBoolean;
       Property ByteField : Byte Read FByte Write FByte;
       Property CharField : Char Read FChar Write FChar;
       Property WordField : Word Read FWord Write FWord;
       Property IntegerField : Integer Read FInteger Write FInteger;
       Property LongintField : Longint Read FLongint Write FLongint;
       Property CardinalField : Cardinal Read FCardinal Write FCardinal;
       Property RealField : Real Read FReal Write FReal;
       Property ExtendedField : Extended Read FExtended Write FExtended;
       Property AnsiStringField : AnsiString Read FAnsiString Write FAnsiString;
       Property MyEnumField : TMyEnum Read FMyEnum Write FMyEnum;
       Property BooleanMethod : Boolean Read GetBoolean Write SetBoolean;
       Property ByteMethod : Byte Read GetByte Write SetByte;
       Property CharMethod : Char Read GetChar Write SetChar;
       Property WordMethod : Word Read GetWord Write SetWord;
       Property IntegerMethod : Integer Read GetInteger Write SetInteger;
       Property LongintMethod : Longint Read GetLongint Write SetLongint;
       Property CardinalMethod : Cardinal Read GetCardinal Write SetCardinal;
       Property RealMethod : Real Read GetReal Write SetReal;
       Property ExtendedMethod : Extended Read GetExtended Write SetExtended;
       Property AnsiStringMethod : AnsiString Read GetAnsiString Write SetAnsiString;
       Property MyEnumMethod : TMyEnum Read GetMyEnum Write SetMyEnum;
       Property BooleanVirtualMethod : Boolean Read GetVirtualBoolean Write SetVirtualBoolean;
       Property ByteVirtualMethod : Byte Read GetVirtualByte Write SetVirtualByte;
       Property CharVirtualMethod : Char Read GetVirtualChar Write SetVirtualChar;
       Property WordVirtualMethod : Word Read GetVirtualWord Write SetVirtualWord;
       Property IntegerVirtualMethod : Integer Read GetVirtualInteger Write SetVirtualInteger;
       Property LongintVirtualMethod : Longint Read GetVirtualLongint Write SetVirtualLongint;
       Property CardinalVirtualMethod : Cardinal Read GetVirtualCardinal Write SetVirtualCardinal;
       Property RealVirtualMethod : Real Read GetVirtualReal Write SetVirtualReal;
       Property ExtendedVirtualMethod : Extended Read GetVirtualExtended Write SetVirtualExtended;
       Property AnsiStringVirtualMethod : AnsiString Read GetVirtualAnsiString Write SetVirtualAnsiString;
       Property MyEnumVirtualMethod : TMyEnum Read GetVirtualMyEnum Write SetVirtualMyEnum;
       Property StoredIntegerConstFalse : Longint Read FLongint Stored False;
       Property StoredIntegerConstTrue : Longint Read FLongint Stored True;
       Property StoredIntegerField : Longint Read FLongint Stored FStored;
       Property StoredIntegerMethod : Longint Read Flongint Stored GetStaticStored;
       Property StoredIntegerVirtualMethod : Longint Read Flongint Stored GetVirtualStored;
       end;

Constructor TMyTestObject.Create;

begin
  FBoolean:=true;
  FByte:=1;        {     : Byte;}
  FChar:='B';      {    : Char; }
  FWord:=3;      {: Word;     }
  FInteger:=4;     {: Integer;  }
  Flongint:=5;     { : Longint; }
  FCardinal:=6;    {: Cardinal; }
  FReal:=7.0;      {     : Real;}
  FExtended :=8.0; { Extended;}
  FMyEnum:=methird; { TMyEnum;}
  FAnsiString:='this is an AnsiString';
  FObj:=TObject.Create;
  FIntf:=TInterfacedObject.Create;
end;

Destructor TMyTestObject.Destroy;

begin
  FObj.Free;
  Inherited Destroy;
end;

Function TMyTestObject.GetBoolean : boolean;

begin
  Result:=FBoolean;
end;

Function TMyTestObject.GetByte : Byte;

begin
  Result:=FByte;
end;

Function TMyTestObject.GetChar : Char;
begin
  Result:=FChar;
end;

Function TMyTestObject.GetWord : Word;
begin
  Result:=FWord;
end;

Function TMyTestObject.GetInteger : Integer;
begin
  Result:=FInteger;
end;

Function TMyTestObject.GetLongint : Longint;
begin
  Result:=FLongint;
end;

Function TMyTestObject.GetCardinal : Cardinal;
begin
  Result:=FCardinal;
end;

Function TMyTestObject.GetReal : Real;
begin
  Result:=FReal;
end;

Function TMyTestObject.GetExtended : Extended;
begin
  Result:=FExtended;
end;

Function TMyTestObject.GetAnsiString : AnsiString;
begin
  Result:=FAnsiString;
end;

Function TMyTestObject.GetMyEnum : TMyEnum;
begin
  Result:=FMyEnum;
end;

Procedure TMyTestObject.Setboolean ( Value : boolean );
begin
  Fboolean:=Value;
end;


Procedure TMyTestObject.SetByte ( Value : Byte );
begin
  FByte:=Value;
end;

Procedure TMyTestObject.SetChar ( Value : Char );
begin
  FChar:=Value;
end;

Procedure TMyTestObject.SetWord ( Value : Word );
begin
  FWord:=Value;
end;

Procedure TMyTestObject.SetInteger ( Value : Integer );
begin
  FInteger:=Value;
end;

Procedure TMyTestObject.SetLongint ( Value : Longint );
begin
  FLongint:=Value;
end;

Procedure TMyTestObject.SetCardinal ( Value : Cardinal );
begin
  FCardinal:=Value;
end;

Procedure TMyTestObject.SetReal ( Value : Real );
begin
  FReal:=Value;
end;

Procedure TMyTestObject.SetExtended ( Value : Extended );
begin
  FExtended:=Value;
end;

Procedure TMyTestObject.SetAnsiString ( Value : AnsiString );
begin
  FAnsiString:=Value;
end;

Procedure TMyTestObject.SetMyEnum ( Value : TMyEnum );
begin
  FMyEnum:=Value;
end;

Function TMyTestObject.GetVirtualBoolean : boolean;

begin
  Result:=FBoolean;
end;

Function TMyTestObject.GetVirtualByte : Byte;

begin
  Result:=FByte;
end;

Function TMyTestObject.GetVirtualChar : Char;
begin
  Result:=FChar;
end;

Function TMyTestObject.GetVirtualWord : Word;
begin
  Result:=FWord;
end;

Function TMyTestObject.GetVirtualInteger : Integer;
begin
  Result:=FInteger;
end;

Function TMyTestObject.GetVirtualLongint : Longint;
begin
  Result:=FLongint;
end;

Function TMyTestObject.GetVirtualCardinal : Cardinal;
begin
  Result:=FCardinal;
end;

Function TMyTestObject.GetVirtualReal : Real;
begin
  Result:=FReal;
end;

Function TMyTestObject.GetVirtualExtended : Extended;
begin
  Result:=FExtended;
end;

Function TMyTestObject.GetVirtualAnsiString : AnsiString;
begin
  Result:=FAnsiString;
end;

Function TMyTestObject.GetVirtualMyEnum : TMyEnum;
begin
  Result:=FMyEnum;
end;

Procedure TMyTestObject.SetVirtualboolean ( Value : boolean );
begin
  Fboolean:=Value;
end;


Procedure TMyTestObject.SetVirtualByte ( Value : Byte );
begin
  FByte:=Value;
end;

Procedure TMyTestObject.SetVirtualChar ( Value : Char );
begin
  FChar:=Value;
end;

Procedure TMyTestObject.SetVirtualWord ( Value : Word );
begin
  FWord:=Value;
end;

Procedure TMyTestObject.SetVirtualInteger ( Value : Integer );
begin
  FInteger:=Value;
end;

Procedure TMyTestObject.SetVirtualLongint ( Value : Longint );
begin
  FLongint:=Value;
end;

Procedure TMyTestObject.SetVirtualCardinal ( Value : Cardinal );
begin
  FCardinal:=Value;
end;

Procedure TMyTestObject.SetVirtualReal ( Value : Real );
begin
  FReal:=Value;
end;

Procedure TMyTestObject.SetVirtualExtended ( Value : Extended );
begin
  FExtended:=Value;
end;

Procedure TMyTestObject.SetVirtualAnsiString ( Value : AnsiString );
begin
  FAnsiString:=Value;
end;

Procedure TMyTestObject.SetVirtualMyEnum ( Value : TMyEnum );
begin
  FMyEnum:=Value;
end;

Function TMyTestObject.GetStaticStored : Boolean;

begin
  Result:=False;
end;

Function TMyTestObject.GetVirtualStored : Boolean;

begin
  Result:=False;
end;

Procedure DumpMem ( PL : PByte );

Var I,j : longint;

begin
  For I:=1 to 16 do
    begin
    Write ((I-1)*16:3,' :');
    For J:=1 to 10  do
      begin
      If (PL^>31) and (PL^<129) then
         Write('  ',CHar(PL^))
      else
        Write (PL^:3);
      Write (' ');
      inc(pl);
      end;
    writeln;
    end;
end;


Function ProcType (PP : Byte) : String;

begin
  Case PP and 3 of
    ptfield   : Result:='from Field';
    ptstatic  : Result:='with static method';
    ptVirtual : Result:='with virtual method';
    ptconst   : Result:='with Const';
   end;
end;

Procedure DumpTypeInfo (O : TMyTestObject);

Var
    PT : PTypeData;
    PI : PTypeInfo;
    I  : Longint;
    PP : PPropList;

begin
  PI:=O.ClassInfo;
  Writeln ('Type kind : ',TypeNames[PI^.Kind]);
  Writeln ('Type name : ',PI^.Name);
  PT:=GetTypeData(PI);
  //DumpMem(PByte(PI));
  If PT^.ParentInfo=Nil then
    Writeln ('Object has no parent info')
  else
    Writeln ('Object has parent info');
  Writeln ('Property Count : ',PT^.PropCount);
  Writeln ('Unit name      : ',PT^.UnitName);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  For I:=0 to PT^.PropCount-1 do
  If PP^[i]<>Nil then
    With PP^[I]^ do
      begin
      Writeln ('Property name : ',Name);
      Writeln (' Type kind: ',TypeNames[PropType^.Kind]);
      Writeln (' Type Name: ',PropType^.Name);
      If GetProc=Nil then Write ('No');
      Writeln (' Getproc available');
      If SetProc=Nil then Write ('No');
      Writeln (' Setproc available');
      If StoredProc=Nil then Write ('No');
      Writeln (' Storedproc available');
      Writeln (' Get property ',proctype(Propprocs));
      Writeln (' Set Property ',proctype(propprocs shr 2));
      Writeln (' Stored Property ',proctype(propprocs shr 4));
      Writeln (' Default : ',Default,' Index : ',Index);
      Writeln (' NameIndex : ',NameIndex);
      end;
    FreeMem (PP);
end;

Procedure PrintObject ( Obj: TMyTestObject);

begin
  With Obj do
    begin
    Writeln ('Field properties :');
    Writeln ('Property ObjField        : ',PtrUInt(ObjField));
    Writeln ('Property IntfField       : ',PtrUInt(IntfField));
    Writeln ('Property booleanField    : ',booleanField);
    Writeln ('Property ByteField       : ',ByteField);
    Writeln ('Property CharField       : ',CharField);
    Writeln ('Property WordField       : ',WordField);
    Writeln ('Property IntegerField    : ',IntegerField);
    Writeln ('Property LongintField    : ',LongintField);
    Writeln ('Property CardinalField   : ',CardinalField);
    Writeln ('Property RealField       : ',RealField);
    Writeln ('Property ExtendedField   : ',ExtendedFIeld);
    Writeln ('Property AnsiStringField : ',AnsiStringField);
    Writeln ('Property MyEnumField     : ',ord(MyEnumField));
    Writeln ('Method properties :');
    Writeln ('Property booleanMethod    : ',BooleanMethod);
    Writeln ('Property ByteMethod       : ',ByteMethod);
    Writeln ('Property CharMethod       : ',CharMethod);
    Writeln ('Property WordMethod       : ',WordMethod);
    Writeln ('Property IntegerMethod    : ',IntegerMethod);
    Writeln ('Property LongintMethod    : ',LongintMethod);
    Writeln ('Property CardinalMethod   : ',CardinalMethod);
    Writeln ('Property RealMethod       : ',RealMethod);
    Writeln ('Property ExtendedMethod   : ',ExtendedMethod);
    Writeln ('Property AnsiStringMethod : ',AnsiStringMethod);
    Writeln ('Property MyEnumMethod     : ',ord(MyEnumMethod));
    Writeln ('VirtualMethod properties :');
    Writeln ('Property booleanVirtualMethod    : ',BooleanVirtualMethod);
    Writeln ('Property ByteVirtualMethod       : ',ByteVirtualMethod);
    Writeln ('Property CharVirtualMethod       : ',CharVirtualMethod);
    Writeln ('Property WordVirtualMethod       : ',WordVirtualMethod);
    Writeln ('Property IntegerVirtualMethod    : ',IntegerVirtualMethod);
    Writeln ('Property LongintVirtualMethod    : ',LongintVirtualMethod);
    Writeln ('Property CardinalVirtualMethod   : ',CardinalVirtualMethod);
    Writeln ('Property RealVirtualMethod       : ',RealVirtualMethod);
    Writeln ('Property ExtendedVirtualMethod   : ',ExtendedVirtualMethod);
    Writeln ('Property AnsiStringVirtualMethod : ',AnsiStringVirtualMethod);
    Writeln ('Property MyEnumVirtualMethod     : ',ord(MyEnumVirtualMethod));
    end;
end;

Procedure TestGet (O : TMyTestObject);

Var
    PT : PTypeData;
    PI : PTypeInfo;
    I,J : Longint;
    PP : PPropList;
    prI : PPropInfo;
    Intf : IInterface;
begin
  PI:=O.ClassInfo;
  Writeln ('Type kind : ',TypeNames[PI^.Kind]);
  Writeln ('Type name : ',PI^.Name);
  PT:=GetTypeData(PI);
  If PT^.ParentInfo=Nil then
    Writeln ('Object has no parent info')
  else
    Writeln ('Object has parent info');
  Writeln ('Property Count : ',PT^.PropCount);
  Writeln ('Unit name      : ',PT^.UnitName);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  For I:=0 to PT^.PropCount-1 do
    begin
    pri:=PP^[i];
    With Pri^ do
      begin
      Write ('(Examining ',name,' : Type : ',TypeNames[PropType^.Kind],', ');
      If (Proptype^.kind in Ordinaltypes) Then
        begin
        J:=GetOrdProp(O,pri);
        Write ('Value : ',j);
        If PropType^.Kind=tkenumeration then
          Write ('(=',GetEnumName(Proptype,J),')')
        end
      else
        Case pri^.proptype^.kind of
          tkfloat :  begin
                     Write ('Value : ');
                     Flush(output);
                     Write(GetFloatProp(O,pri))
                     end;
        tkAstring : begin
                    Write ('value : ');
                    flush (output);
                    Write(GetStrProp(O,Pri));
                    end;
        tkInterface : begin
                       Write ('value : ');
                       flush (output);
                       Write(PtrUInt(GetInterfaceProp(O,Pri)));
                       { play a little bit with the interface to test SetInterfaceProp }
                       SetInterfaceProp(O,Pri,TInterfacedObject.Create);
                     end;
        tkClass   : begin
                       Write ('value : ');
                       flush (output);
                       Write(PtrUInt(GetObjectProp(O,Pri)));
                     end;
        else
          Write ('Untested type:',ord(pri^.proptype^.kind));
        end;
          Writeln (')');
      end;
    end;
  FreeMem (PP);
end;


Var O : TMyTestObject;

begin
  O:=TMyTestObject.Create;
  DumpTypeInfo(O);
  PrintObject(O);
  testget(o);
  O.Free;
end.
