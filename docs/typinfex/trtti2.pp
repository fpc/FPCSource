Program trtti1;

{$Mode Delphi}
{$M+}

Uses
  Rttiobj,Typinfo;

Procedure TestGetPropInfo (O : TMyTestObject);

Const YesNo : Array[Boolean] of string[5] = (' NO ',' YES');

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
      Write('Property: ',Name:26,' (');
      Write(YesNo[GetPropInfo(PI,Name)=PP^[i]]);
      Write(YesNo[GetPropInfo(O,Name)=PP^[I]]);
      Write(YesNo[GetPropInfo(O.ClassType,Name)=PP^[I]]);
      Write(YesNo[GetPropInfo(PI,Name,tkProperties)=PP^[i]]);
      Write(YesNo[GetPropInfo(O,Name,tkProperties)=PP^[I]]);
      Write(YesNo[GetPropInfo(O.ClassType,Name,tkProperties)=PP^[I]]);
      Write(YesNo[FindPropInfo(O,Name)=PP^[I]]);
      Write(YesNo[FindPropInfo(O.ClassType,Name)=PP^[I]]);
      Writeln(')')
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
  TestGetPropInfo(O);
//  testget(o);
end.
