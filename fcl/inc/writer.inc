{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************}
{*                             TAbstractWriter                                      *}
{****************************************************************************}

{ $define serdebug}

Procedure TAbstractWriter.AddAncestor(Component: TComponent);

begin
  FAncestorList.Add(Component);
end;

Procedure TAbstractWriter.WriteData(Instance: TComponent);

begin
{$ifdef serdebug}
  Writeln(stderr,'Writer: Starting WriteData');
{$endif}
  With Instance do
    StartObject(ClassName,Name);
  WriteProperties(Instance);
  Instance.GetChildren(@WriteComponent,FRoot);
  EndObject;
end;

{
  These methods do the main work: decide if a property must be written,
  and then call the write method.
  Later on the NeedsWriting function should take the ancestor into
  account as well, for form inheritance...
}


Procedure TAbstractWriter.DoOrdinalProp(Instance : TPersistent;Propinfo :PPropInfo);

Var
  Value : longint;

begin
  {$ifdef serdebug}
    Writeln(stderr,'Writer: Starting DoOrdinalProp');
  {$endif}
  Value:=GetOrdProp(Instance,Propinfo);
  If Value<>(PropInfo^.default) then
    With PropInfo^ do
      Case PropType^.Kind of
        tkInteger :  WriteIntegerProperty(Name,Value);
        tkSet : WriteSetProperty (Name,Value,GetTypeData(Proptype)^.CompType^);
        tkEnumeration : WriteEnumerationProperty (Name,Value,GetEnumName(Proptype,Value));
      end;
end;

Procedure TAbstractWriter.DoStringProp(Instance : TPersistent;Propinfo :PPropInfo);

Var Value : String;

begin
  {$ifdef serdebug}
    Writeln(stderr,'Writer: Starting DoStringProp');
  {$endif}
  Value:=GetStrProp(Instance,PropInfo);
  If Value<>'' Then
    With Propinfo^ do
      WriteStringProperty(Name,Value);
end;

Procedure TAbstractWriter.DoFloatProp(Instance : TPersistent;Propinfo :PPropInfo);

Var Value : Extended;

begin
  {$ifdef serdebug}
    Writeln(stderr,'Writer: Starting DoFloatProp');
  {$endif}
  Value:=GetFloatProp(Instance,Propinfo);
  If (Value<>0.0) then
    With PropInfo^ do
      WriteFloatProperty(Name,Value);
end;


Procedure TAbstractWriter.DoCollectionProp(Name: ShortString; Value : TCollection);

Var OldPrefix : String;

begin
  {$ifdef serdebug}
    Writeln(stderr,'Writer: Starting DoCollectionProp');
  {$endif}
  Try
    OldPrefix:=FPrefix;
    FPrefix:='';
    WriteCollectionProperty(Name,Value)
  Finally
    FPrefix:=OldPrefix;
  end;
end;


Procedure TAbstractWriter.DoClassProp(Instance : TPersistent;Propinfo :PPropInfo);

{
  Some explanation:
  1) Only TPersistent properties can be written, since higher has no
     RTTI (actually, we could test if the class has RTTI if it isn't
     TPersistent, but Delphi doesn't - We can add it later)
  2) If it is a TPersistent but not TComponent, then the only
     thing that is (can be) written is the defineproperties;
     we have this handled by calling writeproperties again.
  3) When a property is a TComponent, it is owned by the form or by a
     TDataModule; This means that the component is streamed also
     (owner-owned) by the form, so it is sufficient to store a reference
     to the component, not store the component itself.

     Again, this is very form-oriented; at a later stage, we should see
     to make this more broader.

}

Var
  Value : TObject;

  Function NeedsWriting : Boolean;

  begin
    Result:=Value<>Nil;
  end;

  Function GetComponentPath(Component : TComponent): String;

  begin
    If Component.Owner=Root Then
      Result:=Component.Name          // 2 objects In the same form.
    else if Component=Root then
      Result:='Owner'                 // Component = Form.
    else if Component.Owner<>Nil then
      Result:=Format('%s.%s',[Component.Owner.name,Component.Name]) // Component on other e.g. Datamodule.
    else
      Result:=Format('%s.%s',[Component.Name+'owner']); // All other cases.
  end;

Var
  OldPrefix,CName : String;

begin
{$ifdef serdebug}
  Writeln(stderr,'Writer: Starting DoClassProp');
{$endif}
  Value:=TObject(GetOrdProp(Instance,PropInfo));  // get as pointer
{$ifdef serdebug}
  If Value=Nil then
    Writeln(stderr,'Writer: value is nil');
  Writeln(stderr,'name ',propinfo^.Name);
{$endif}
  If (Value=Nil) Then
    begin
    If Needswriting then
      With Propinfo^ do
        WriteNilProperty(Name)
    end
  else
    If Value is TPersistent then
      begin
{$ifdef serdebug}
  Writeln(stderr,'Writer: value is tpersistent');
{$endif}
        If Value is TComponent then
          { Component is written by itself,
            just write a reference }
          begin
          Cname:=GetComponentPath(TComponent(Value));
          If NeedsWriting and (Cname<>'') then
            begin
              With PropInfo^ do
                WriteComponentProperty(Name,TComponent(Value));
            end;
          end
        else If Value is TCollection then
           DoCollectionProp(Propinfo^.Name,TCollection(Value))
        else
          With Propinfo^ do
            begin  // TPersistent, not TComponent.
            OldPrefix:=FPrefix;
            FPrefix:=Format('%s%s.',[OldPrefix,Name]); // eg. Memo.Lines.Strings !
            try
              WriteProperties(TPersistent(Value));
            finally
              FPrefix:=OldPrefix;
            end;
          end;
      end
   // We can't write it if it isn't a TPersistent...
end;

Procedure TAbstractWriter.DoMethodProp(Instance : TPersistent;Propinfo :PPropInfo);
{

  Some explanation: AFAIK Delphi only allows to assign methods from the
  current form to an event. (An event is a Method) this means that the
  instance part of the method IS the Form which IS the 'root' component.
  this means that we can safely assume that Method.Data = Root...

  Remark also that Form Methods are always in a Published section of the form,
  Since Delphi manages them, hence the method name is always in RTTI.

  If we want a more general streaming method (i.e. not form oriented) then
  we would have to write ComponentPath.MethodName or something.
}

Var
  Value : TMethod;

begin
{$ifdef serdebug}
  Writeln(stderr,'Writer: Starting DoMethodProp');
{$endif}
  Value:=GetMethodProp(Instance,Propinfo);
  With Value do
    If Code<>Nil then
      WriteMethodProperty(Propinfo^.Name,Root.MethodName(Code));
end;

Procedure TAbstractWriter.WriteProperty(Instance: TPersistent; PropInfo: Pointer);

{$ifdef serdebug}
Const
 TypeNames : Array [TTYpeKind] of string[15] =
            ('Unknown','Integer','Char','Enumeration',
             'Float','Set','Method','ShortString','LongString',
             'AnsiString','WideString','Variant','Array','Record',
             'Interface','Class','Object','WideChar','Bool');

 Const OrdinalTypes = [tkInteger,tkChar,tkENumeration,tkbool];
{$endif}

begin
{$ifdef serdebug}
  Writeln(stderr,'Writer: Starting WriteProperty');
  With PPropInfo(Propinfo)^ do
    begin
    Writeln (stderr,' Type kind: ',TypeNames[PropType^.Kind]);
    Writeln (stderr,' Type Name: ',PropType^.Name);
    Writeln (stderr,'Writer: Starting WriteProperty');
    end;
{$endif}
  // Dispatching routine. For compatibility only.
  With PPropinfo(Propinfo)^ do
    Case PropType^.Kind of
      tkchar,tkInteger,tkenumeration,tkset : DoOrdinalProp(Instance,Propinfo);
      tkAstring,tkstring,tkLString,tkWstring : DoStringProp(Instance,Propinfo);
      tkfloat : DoFloatProp(Instance,PropInfo);
      tkClass : DoClassProp(Instance,PropInfo);
      tkMethod : DoMethodProp(Instance,PropInfo);
    end;
end;


Procedure TAbstractWriter.WriteProperties(Instance: TPersistent);

Var I,PropCount : Longint;
    Props : PPropList;

begin
{$ifdef serdebug}
  Writeln(stderr,'Writer: Starting WriteProperties');
{$endif}
  PropCount:=GetTypeData(Instance.ClassInfo)^.PropCount;
{$ifdef serdebug}
  Writeln(stderr,'Writer : Propcount: ',PropCount);
{$endif}
  Try
    GetMem (Props,SizeOf(Pointer)*PropCount);
    GetPropInfos(Instance.ClassInfo,Props);
    For I:=0 to PropCount-1 do
      WriteProperty(Instance,Props^[I]);
  finally
    FreeMem(Props);
  end;
//  Instance.DefineProperties(Self);
end;

Destructor TAbstractWriter.Destroy;

begin
end;

Procedure TAbstractWriter.WriteDescendent(ARoot: TComponent; AAncestor: TComponent);

begin
{$ifdef serdebug}
  Writeln(stderr,'Writer: Starting WriteDescendent');
{$endif}
  FRootAncestor:=AAncestor;
  FAncestor:=Ancestor;
  FRoot:=ARoot;
  WriteComponent(ARoot)
end;


Procedure TAbstractWriter.WriteRootComponent(ARoot: TComponent);

begin
{$ifdef serdebug}
  Writeln(stderr,'Writer: Starting WriteRootComponent');
{$endif}
  WriteDescendent(ARoot,Nil);
end;

procedure TAbstractWriter.WriteComponent(Component: TComponent);

Var I : longint;
    TheAncestor : TComponent;

begin
{$ifdef serdebug}
  Writeln(stderr,'Writer: Starting WriteComponent');
{$endif}
  Include(Component.FComponentState,csWriting);
  TheAncestor:=Nil;
  If Assigned(FAncestorList) then
    For I:=0 to FAncestorList.Count-1 do
      If TComponent(FAncestorList[i]).Name=Component.Name then
        begin
        TheAncestor:=Tcomponent(FancestorList[i]);
        break;
        end;
  Ancestor:=TheAncestor;
  Component.WriteState(Self);
  Exclude(Component.FComponentState,csWriting);
end;


{ ---------------------------------------------------------------------
    TWriter Methods
  ---------------------------------------------------------------------}

Constructor TWriter.Create(S : TStream);

begin
  FStream:=S;
end;


Destructor TWriter.Destroy;

begin
end;

Procedure TWriter.FlushBuffer;

begin
  // For compatibility only.
end;


Procedure TWriter.Write(const Buf; Count: Longint);

begin
  FStream.Write(Buf,Count);
end;

Procedure TWriter.WriteIntegerProperty(Const Name : Shortstring;Value : Longint);

begin
  WritePropName(Name);
  WriteInteger(Value);
end;


Procedure TWriter.WriteSetProperty (Const Name : ShortString;Value : longint; BaseType : TTypeInfo);

begin
  WritePropName(Name);
end;


Procedure TWriter.WriteEnumerationProperty (Const Name : ShortString;Value : Longint; Const EnumName : ShortSTring);

begin
  WritePropName(Name);
  WriteIdent(EnumName);
end;


Procedure TWriter.WriteStringProperty(Const Name : ShortString; Const Value : String);

begin
  WritePropName(Name);
  WriteString(Value);
end;


Procedure TWriter.WriteFloatProperty(Const Name : ShortString; Value : Extended);

begin
  WritePropName(Name);
  WriteFloat(Value);
end;


Procedure TWriter.WriteCollectionProperty(Const Name : ShortString;Value : TCollection);

begin
end;


Procedure TWriter.WriteClassProperty(Instance : TPersistent;Propinfo :PPropInfo);

begin
end;


Procedure TWriter.WriteComponentProperty(Const Name : ShortString; Value : TComponent);

begin
  WritePropName(Name);
  WriteIdent(Value.Name);
end;


Procedure TWriter.WriteNilProperty(Const Name : Shortstring);

begin
  WritePropName(Name);
  WriteValue(vaNil)
end;


Procedure TWriter.WriteMethodProperty(Const Name,AMethodName : Shortstring);

begin
end;


procedure TWriter.WriteBoolean(Value: Boolean);

begin
  If Value then WriteValue(vaTrue) else WriteValue(vaFalse)
end;


procedure TWriter.WriteCollection(Value: TCollection);

begin
end;


procedure TWriter.WriteChar(Value: Char);

begin
end;


procedure TWriter.WriteFloat(Value: Extended);

begin
end;


procedure TWriter.WriteIdent(const Ident: string);

begin
  if (Ident='Nil') then WriteValue(vaNil) else
  if (Ident='True') then WriteValue(vaTrue) else
  If (Ident='False') then WriteValue(vaFalse) else
    begin
    WriteValue(vaIdent);
    WriteStr(Ident);
    end
end;


procedure TWriter.WriteInteger(Value: Longint);

begin
  If (Value>=-128) and (Value<=127) then
    begin
    WriteValue(vaInt8);
    Write(Value,SizeOf(ShortInt));
    end
  else If (Value>=-32768) and (Value<=32767) then
    begin
    WriteValue(vaInt16);
    Write(Value,SizeOf(SmallInt));
    end
 else
   begin
   WriteValue(vaInt32);
   Write(Value,SizeOf(Longint));
   end;
end;


procedure TWriter.WriteListBegin;

begin
  WriteValue(vaList);
end;


procedure TWriter.WriteListEnd;

begin
  WriteValue(vaNull)
end;


procedure TWriter.WriteSignature;

begin
  Write(FilerSignature,SizeOf(FilerSignature));
end;


procedure TWriter.WriteStr(const Value: string);

Var L : longint;

begin
  L:=Length(Value);
  If L>255 then
    L:=255;
  Write(L,SizeOf(Byte));
  Write(Pointer(Value)^,L);
end;


procedure TWriter.WriteString(const Value: string);

Var L : longint;

begin
  L:=Length(Value);
  If L<=255 then
    begin
    WriteValue(vastring);
    Write(L,SizeOf(Byte));
    end
  else
    begin
    WriteValue(vaLstring);
    Write(L,SizeOf(Longint))
    end;
  Write(Pointer(Value)^,L);
end;

Procedure TWriter.WritePrefix(Flags: TFilerFlags; AChildPos: Integer);

begin
end;

Procedure TWriter.WriteValue(Value : TValueType);

begin
  Write(Value,SizeOf(Value));
end;

Procedure TWriter.WriteBuffer;

begin
  // For compatibility only.
end;

function  TWriter.GetPosition: Longint;

begin
  GetPosition:=0;
end;


Procedure TWriter.SetPosition(Value: Longint);

begin
end;

Procedure TWriter.WriteBinary(wd : TStreamProc);

begin
end;

Procedure TWriter.WritePropName(const PropName: string);

begin
  WriteStr(PropName)
end;

Procedure TWriter.DefineProperty(const Name: string;
  rd : TReaderProc; wd : TWriterProc;
  HasData: Boolean);

begin
end;


Procedure TWriter.DefineBinaryProperty(const Name: string;
  rd, wd: TStreamProc;
  HasData: Boolean);

begin
end;

Procedure TAbstractWriter.DefineProperty(const Name: string;
  rd : TReaderProc; wd : TWriterProc;
  HasData: Boolean);

begin
end;


Procedure TAbstractWriter.DefineBinaryProperty(const Name: string;
  rd, wd: TStreamProc;
  HasData: Boolean);

begin
end;

{
  $Log$
  Revision 1.6  2000-01-07 01:24:33  peter
    * updated copyright to 2000

  Revision 1.5  2000/01/06 01:20:33  peter
    * moved out of packages/ back to topdir

  Revision 1.2  2000/01/04 18:07:16  michael
  + Streaming implemented

  Revision 1.3  1999/09/13 08:35:16  fcl
  * Changed some argument names (Root->ARoot etc.) because the new compiler
    now performs more ambiguity checks  (sg)

  Revision 1.2  1999/04/08 10:18:58  peter
    * makefile updates

}
