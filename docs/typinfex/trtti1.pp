Program trtti1;

{$Mode Delphi}
{$M+}

Uses
  Rttiobj,Typinfo;

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
        else
          Write ('Untested type:',ord(pri^.proptype^.kind));
        end;
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
