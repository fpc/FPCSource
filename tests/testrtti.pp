Program testrtti;

Uses Typinfo,classes;

Const TypeNames : Array [TTYpeKind] of string[15] = 
                    ('Unknown','Integer','Char','Enumeration',
                     'Float','Set','Method','ShortString','LongString',
                     'AnsiString','WideString','Variant','Array','Record',
                     'Interface','Class','Object','WideChar','Bool');

Const OrdinalTypes = [tkInteger,tkChar,tkENumeration,tkbool];

Type
      TMyEnum = (meFirst,meSecond,meThird);

Type TMyTestObject = Class(TPersistent)
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
       Public
       Constructor Create;
       Destructor Destroy;override;       
       Published
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
end;

Destructor TMyTestObject.Destroy;

begin
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
    I,J : Longint;
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
      Writeln (' Stored Property ',proctype(propprocs shr 2));
      Writeln (' Default : ',Default,' Index : ',Index);
      Writeln (' NameIndex : ',NameIndex);
      end;
end;

Procedure PrintObject ( Obj: TMyTestObject);

begin
  With Obj do
    begin
    Writeln ('Field properties :');
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
    end;
end;

Procedure TestGet (O : TMyTestObject);

Var
    PT : PTypeData;
    PI : PTypeInfo;
    I,J : Longint;
    PP : PPropList;
    prI : PPropInfo;    
    
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
        Write ('Value : ',GetOrdProp(O,pri))
{
      // Skipping the ord() never gives True !!! ???
      else If ord(pri^.proptype^.kind) = ord(tkfloat) then
        begin
        Write (', Value : ');
        Flush(output);
        Write(GetFloatProp(O,pri))
        end
}
      else
        Write ('Not of type ordinal, bool or float:',ord(pri^.proptype^.kind));
      Writeln (')');
      end;
    end;
end;

Var O : TMyTestObject;

begin
  O:=TMyTestObject.Create;
  DumpTypeInfo(O);
  PrintObject(O);
  testget(o);
end.