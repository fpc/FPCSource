{
    pas2jni - JNI bridge generator for Pascal.

    Copyright (c) 2013 by Yury Sidorov.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}

unit writer;

{$mode objfpc}{$H+}

interface

//{$define DEBUG}

{$ifdef DEBUG}
{$ASSERTIONS ON}
{$endif}

uses
  Classes, SysUtils, def, contnrs, PPUParser;

const
  MaxMethodPointers = 10000;

type
  { TTextOutStream }

  TTextOutStream = class(TFileStream)
  private
    FIndent: integer;
    FIndStr: string;
    procedure SetIndednt(const AValue: integer);
  public
    procedure Write(const s: ansistring); overload;
    procedure WriteLn(const s: ansistring = ''; ExtraIndent: integer = 0);
    procedure IncI;
    procedure DecI;
    property Indent: integer read FIndent write SetIndednt;
    property SIndent: string read FIndStr;
  end;

  { TWriter }

  TWriter = class
  private
    Fjs, Fps: TTextOutStream;
    FClasses: TStringList;
    FPkgDir: string;
    FUniqueCnt: integer;
    FThisUnit: TUnitDef;

    function DoCheckItem(const ItemName: string): TCheckItemResult;

    procedure ProcessRules(d: TDef; const Prefix: string = '');
    function GetUniqueNum: integer;
    function DefToJniType(d: TDef; var err: boolean): string;
    function DefToJniSig(d: TDef): string;
    function DefToJavaType(d: TDef): string;
    function GetJavaClassPath(d: TDef; const AClassName: string = ''): string;
    function JniToPasType(d: TDef; const v: string; CheckNil: boolean): string;
    function PasToJniType(d: TDef; const v: string): string;
    function GetTypeInfoVar(ClassDef: TDef): string;
    function GetClassPrefix(ClassDef: TDef; const AClassName: string = ''): string;
    function IsJavaSimpleType(d: TDef): boolean;
    function GetProcDeclaration(d: TProcDef; const ProcName: string = ''): string;
    function GetJavaProcDeclaration(d: TProcDef; const ProcName: string = ''): string;
    function GetJniFuncType(d: TDef): string;
    function GetJavaClassName(cls: TDef; it: TDef): string;
    procedure RegisterPseudoClass(d: TDef);
    function GetPasIntType(Size: integer): string;
//    procedure AddCustomProc(ParentDef: TDef; const JniName, Name: string; RetType: TBasicType; const Params: array of TBasicType);
    function AddCustomProc(ParentDef: TDef; const JniName, Name: string; RetType: TBasicType; const Params: array of TBasicType): TProcDef;
    procedure AddNativeMethod(ParentDef: TDef; const JniName, Name, Signature: string);
    function GetProcSignature(d: TProcDef): string;
    procedure EHandlerStart;
    procedure EHandlerEnd(const EnvVarName: string; const ExtraCode: string = '');

    procedure WriteClassInfoVar(d: TDef);
    procedure WriteComment(d: TDef; const AType: string);
    procedure WriteClass(d: TDef; PreInfo: boolean);
    procedure WriteProc(d: TProcDef; Variable: TVarDef = nil; AParent: TDef = nil);
    procedure WriteVar(d: TVarDef; AParent: TDef = nil);
    procedure WriteConst(d: TConstDef);
    procedure WriteEnum(d: TDef);
    procedure WriteProcType(d: TProcDef; PreInfo: boolean);
    procedure WriteSet(d: TSetDef);
    procedure WriteUnit(u: TUnitDef);
    procedure WriteOnLoad;
  public
    SearchPath: string;
    LibName: string;
    JavaPackage: string;
    Units: TStringList;
    OutPath: string;
    JavaOutPath: string;
    IncludeList: TStringList;
    ExcludeList: TStringList;

    constructor Create;
    destructor Destroy; override;
    procedure ProcessUnits;
  end;

implementation

const
  JNIType: array[TBasicType] of string =
    ('', 'jshort', 'jbyte', 'jint', 'jshort', 'jlong', 'jint', 'jlong', 'jfloat', 'jdouble', 'jstring',
     'jstring', 'jboolean', 'jchar', 'jchar', 'jint', 'jlong', 'jstring');
  JNITypeSig: array[TBasicType] of string =
    ('V', 'S', 'B', 'I', 'S', 'J', 'I', 'J', 'F', 'D', 'Ljava/lang/String;', 'Ljava/lang/String;',
     'Z', 'C', 'C', 'I', 'J', 'Ljava/lang/String;');
  JavaType: array[TBasicType] of string =
    ('void', 'short', 'byte', 'int', 'short', 'long', 'int', 'long', 'float', 'double', 'String',
     'String', 'boolean', 'char', 'char', 'int', 'long', 'String');

  TextIndent = 2;

  ExcludeStd: array[1..44] of string = (
    'classes.TStream.ReadComponent', 'classes.TStream.ReadComponentRes', 'classes.TStream.WriteComponent', 'classes.TStream.WriteComponentRes',
    'classes.TStream.WriteDescendent', 'classes.TStream.WriteDescendentRes', 'classes.TStream.WriteResourceHeader', 'classes.TStream.FixupResourceHeader',
    'classes.TStream.ReadResHeader', 'classes.TComponent.WriteState', 'classes.TComponent.ExecuteAction', 'classes.TComponent.UpdateAction',
    'classes.TComponent.GetEnumerator', 'classes.TComponent.VCLComObject', 'classes.TComponent.DesignInfo', 'classes.TComponent.Destroying',
    'classes.TComponent.FreeNotification', 'classes.TComponent.RemoveFreeNotification', 'classes.TComponent.FreeOnRelease', 'classes.TComponent.SetSubComponent',
    'system.TObject.newinstance', 'system.TObject.FreeInstance', 'system.TObject.SafeCallException', 'system.TObject.InitInstance',
    'system.TObject.CleanupInstance', 'system.TObject.ClassInfo', 'system.TObject.AfterConstruction', 'system.TObject.BeforeDestruction',
    'system.TObject.GetInterfaceEntry', 'system.TObject.GetInterfaceTable', 'system.TObject.MethodAddress', 'system.TObject.MethodName',
    'system.TObject.FieldAddress', 'classes.TComponent.ComponentState', 'classes.TComponent.ComponentStyle', 'classes.TList.GetEnumerator',
    'classes.TList.List', 'classes.TList.FPOAttachObserver', 'classes.TList.FPODetachObserver', 'classes.TList.FPONotifyObservers',
    'classes.TPersistent.FPOAttachObserver', 'classes.TPersistent.FPODetachObserver', 'classes.TPersistent.FPONotifyObservers',
    'system.fma'
  );

  ExcludeDelphi7: array[1..25] of string = (
    'system.TObject.StringMessageTable', 'system.TObject.GetInterfaceEntryByStr', 'system.TObject.UnitName', 'system.TObject.Equals',
    'system.TObject.GetHashCode', 'system.TObject.ToString','classes.TStream.ReadByte', 'classes.TStream.ReadWord',
    'classes.TStream.ReadDWord', 'classes.TStream.ReadQWord', 'classes.TStream.ReadAnsiString', 'classes.TStream.WriteByte',
    'classes.TStream.WriteWord', 'classes.TStream.WriteDWord', 'classes.TStream.WriteQWord', 'classes.TStream.WriteAnsiString',
    'classes.TCollection.Exchange', 'classes.TStrings.Equals', 'classes.TStrings.GetNameValue', 'classes.TStrings.ExtractName',
    'classes.TStrings.TextLineBreakStyle', 'classes.TStrings.StrictDelimiter', 'classes.TStrings.GetEnumerator', 'classes.TStringList.OwnsObjects',
    'classes.TList.AddList'
  );

  SUnsupportedType = '<unsupported type>';

function JniCaliing: string;
begin
  Result:='{$ifdef windows} stdcall {$else} cdecl {$endif};';
end;

{ TTextOutStream }

procedure TTextOutStream.SetIndednt(const AValue: integer);
begin
  if FIndent = AValue then exit;
  FIndent:=AValue;
  SetLength(FIndStr, FIndent*TextIndent);
  if FIndent > 0 then
    FillChar(FIndStr[1], FIndent*TextIndent, ' ');
end;

procedure TTextOutStream.Write(const s: ansistring);
begin
  WriteBuffer(PChar(s)^, Length(s));
end;

procedure TTextOutStream.WriteLn(const s: ansistring; ExtraIndent: integer);
begin
  if s = '' then
    Write(LineEnding)
  else begin
    Indent:=Indent + ExtraIndent;
    try
      Write(FIndStr + s + LineEnding);
    finally
      Indent:=Indent - ExtraIndent;
    end;
  end;
end;

procedure TTextOutStream.IncI;
begin
  Indent:=Indent + 1;
end;

procedure TTextOutStream.DecI;
begin
  if Indent > 0  then
    Indent:=Indent - 1;
end;

type
  { TClassInfo }
  TClassInfo = class
  public
    Def: TDef;
    Funcs: TObjectList;
    IsCommonClass: boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TProcInfo = class
  public
    Name: string;
    JniName: string;
    JniSignature: string;
  end;

{ TClassInfo }

constructor TClassInfo.Create;
begin
  Funcs:=TObjectList.Create(True);
end;

destructor TClassInfo.Destroy;
begin
  Funcs.Free;
  inherited Destroy;
end;

{ TWriter }

function TWriter.DefToJniType(d: TDef; var err: boolean): string;
begin
  if d = nil then begin
    Result:=SUnsupportedType;
    err:=True;
  end
  else begin
    if not d.IsUsed then begin
      Result:='<excluded type> ' + d.Name;
      err:=True;
    end
    else
      case d.DefType of
        dtType:
          Result:=JNIType[TTypeDef(d).BasicType];
        dtClass, dtRecord, dtEnum:
          Result:='jobject';
        dtProcType:
          if poMethodPtr in TProcDef(d).ProcOpt then
            Result:='jobject'
          else begin
            Result:=SUnsupportedType + ' ' + d.Name;
            err:=True;
          end;
        dtSet:
          if TSetDef(d).Size <= 4 then
            Result:='jobject'
          else begin
            Result:=SUnsupportedType + ' ' + d.Name;
            err:=True;
          end;
        else begin
          Result:=SUnsupportedType + ' ' + d.Name;
          err:=True;
          d.SetNotUsed;
        end;
    end;
  end;
end;

function TWriter.DoCheckItem(const ItemName: string): TCheckItemResult;
begin
  if IncludeList.IndexOf(ItemName) >= 0 then
    Result:=crInclude
  else
    if ExcludeList.IndexOf(ItemName) >= 0 then
      Result:=crExclude
    else
      Result:=crDefault;
end;

procedure TWriter.ProcessRules(d: TDef; const Prefix: string);
var
  i: integer;
  s: string;
begin
  s:=Prefix + d.Name;
  i:=IncludeList.IndexOf(s);
  if i >= 0 then begin
    i:=ptruint(IncludeList.Objects[i]);
    if (i = 0) or (d.Count = i - 1) then
      d.IsUsed:=True;
  end
  else
    if ExcludeList.IndexOf(s) >= 0 then begin
      d.SetNotUsed;
    end;
  if not (d.DefType in [dtUnit, dtClass, dtRecord]) then
    exit;
  s:=s + '.';
  for i:=0 to d.Count - 1 do
    ProcessRules(d[i], s);
end;

function TWriter.GetUniqueNum: integer;
begin
  Inc(FUniqueCnt);
  Result:=FUniqueCnt;
end;

function TWriter.DefToJniSig(d: TDef): string;
begin
  if d = nil then
    Result:=SUnsupportedType
  else
    case d.DefType of
      dtType:
        Result:=JNITypeSig[TTypeDef(d).BasicType];
      dtClass, dtRecord, dtProcType, dtSet, dtEnum:
        Result:='L' + GetJavaClassPath(d) + ';';
      else
        Result:=SUnsupportedType;
    end;
end;

function TWriter.DefToJavaType(d: TDef): string;
begin
  if d = nil then
    Result:=SUnsupportedType
  else
    case d.DefType of
      dtType:
        Result:=JavaType[TTypeDef(d).BasicType];
      dtClass, dtRecord, dtProcType, dtSet, dtEnum:
        Result:=d.Name;
      else
        Result:=SUnsupportedType;
  end;
end;

function TWriter.GetJavaClassPath(d: TDef; const AClassName: string): string;
var
  n: string;
begin
  if AClassName = '' then
    n:=d.AliasName
  else
    n:=AClassName;
  Result:=StringReplace(JavaPackage, '.', '/', [rfReplaceAll]);
  if Result <> '' then
    Result:=Result + '/';
  if d.DefType = dtUnit then
    Result:=Result + n
  else
    Result:=Result + d.Parent.AliasName + '$' + n;
end;

procedure TWriter.WriteClass(d: TDef; PreInfo: boolean);
var
  WrittenItems: TList;

  procedure _WriteConstructors(c: TClassDef; Written: TStringList);
  var
    i, j: integer;
    p: TProcDef;
    OldRet: TDef;
    s: string;
  begin
    if c = nil then
      exit;
    for i:=0 to c.Count - 1 do
      with c[i] do begin
        if (DefType = dtProc) and not c.IsPrivate and (TProcDef(c[i]).ProcType = ptConstructor) then begin
          p:=TProcDef(c[i]);
          j:=Written.IndexOf(p.Name);
          if (j < 0) or (Written.Objects[j] = c) then begin
            s:=p.Name + ':';
            for j:=0 to p.Count - 1 do
              s:=s + DefToJniSig(p[j]);
            if Written.IndexOf(s) < 0 then begin
              OldRet:=p.ReturnType;
              p.ReturnType:=d;
              p.Parent:=d;
              try
                WriteProc(p);
              finally
                p.ReturnType:=OldRet;
                p.Parent:=c;
              end;
              Written.Add(s);
              if not (poOverload in p.ProcOpt) then
                Written.AddObject(p.Name, c);
            end;
          end;
        end;
      end;

    _WriteConstructors(c.AncestorClass, Written);
  end;

  procedure WriteConstructors;
  var
    cc: TStringList;
  begin
    if not TClassDef(d).HasAbstractMethods then begin
      // Writing all constructors including parent's
      cc:=TStringList.Create;
      try
        cc.Sorted:=True;
        _WriteConstructors(TClassDef(d), cc);
      finally
        cc.Free;
      end;
    end;
  end;

  procedure _WriteReplacedItems(c: TClassDef);
  var
    i: integer;
    p: TReplDef;
  begin
    c:=c.AncestorClass;
    if c = nil then
      exit;
    if c.HasReplacedItems then begin
      for i:=0 to c.Count - 1 do
        with c[i] do begin
          p:=TReplDef(c[i]);
          if (DefType in ReplDefs) and ((p.IsReplaced) or p.IsReplImpl) then begin
            if p.ReplacedItem <> nil then
              WrittenItems.Add(p.ReplacedItem);
            if WrittenItems.IndexOf(p) >= 0 then
              continue;
            case p.DefType of
              dtProc:
                WriteProc(TProcDef(p), nil, d);
              dtProp, dtField:
                WriteVar(TVarDef(p), d);
            end;
          end;
        end;
    end;
    _WriteReplacedItems(c);
  end;

  procedure WriteReplacedItems;
  begin
    _WriteReplacedItems(TClassDef(d));
  end;

  procedure WriteItems(Regular, Replaced, ReplImpl: boolean);
  var
    i: integer;
    it: TReplDef;
  begin
    for i:=0 to d.Count - 1 do begin
      it:=TReplDef(d[i]);
      if not (it.DefType in ReplDefs) then
        continue;
      if not (it.IsReplImpl or it.IsReplaced) then begin
        if not Regular then
          continue;
      end
      else
        if (not Replaced and it.IsReplaced) or (not ReplImpl and it.IsReplImpl) then
          continue;
      if it.ReplacedItem <> nil then
        WrittenItems.Add(it.ReplacedItem);
      case it.DefType of
        dtProc:
          if TProcDef(it).ProcType <> ptConstructor  then
            WriteProc(TProcDef(it));
        dtProp, dtField:
          WriteVar(TVarDef(it));
      end;
    end;
  end;

var
  s, ss: string;
  RegularClass: boolean;
begin
  if PreInfo then begin
    WriteClassInfoVar(d);

    if d.DefType = dtRecord then begin
      s:=d.Parent.Name + '.' + d.Name;
      Fps.WriteLn;
      Fps.WriteLn(Format('function _%s_CreateObj(env: PJNIEnv; const r: %s): jobject;', [GetClassPrefix(d), s]));
      Fps.WriteLn(Format('var pr: ^%s;', [s]));
      Fps.WriteLn('begin');
      Fps.IncI;
      Fps.WriteLn('New(pr); pr^:=r;');
      Fps.WriteLn(Format('Result:=_CreateJavaObj(env, pr, %s);', [GetTypeInfoVar(d)]));
      Fps.DecI;
      Fps.WriteLn('end;');

      Fps.WriteLn;
      ss:=Format('_%s_Free', [GetClassPrefix(d)]);
      Fps.WriteLn(Format('procedure %s(env: PJNIEnv; _self: JObject; r: jlong);', [ss]) + JniCaliing);
      Fps.WriteLn(Format('var pr: ^%s;', [s]));
      Fps.WriteLn('begin');
      Fps.WriteLn('pr:=pointer(ptruint(r));', 1);
      Fps.WriteLn('Dispose(pr);', 1);
      Fps.WriteLn('end;');

      AddNativeMethod(d, ss, 'Release', '(J)V');
    end;
    exit;
  end;

  // Java
  case d.DefType of
    dtClass:
      s:='class';
    dtRecord:
      s:='record';
    else
      s:='';
  end;
  WriteComment(d, s);
  s:='public static class ' + GetJavaClassName(d, nil) + ' extends ';
  if d.DefType = dtClass then
    with TClassDef(d) do begin
      if AncestorClass <> nil then begin
        ss:=AncestorClass.Name;
        if ImplementsReplacedItems then
          ss:='__' + ss;
        s:=s + ss;
      end
      else
        s:=s + 'PascalObject';
    end
    else
      s:=s + Format('%s.system.Record', [JavaPackage]);
  Fjs.WriteLn(s + ' {');
  Fjs.IncI;
  if d.DefType = dtRecord then begin
    Fjs.WriteLn('private native void Release(long pasobj);');
    Fjs.WriteLn(Format('public %s() { }', [d.Name]));
    Fjs.WriteLn(Format('public void Free() { Release(_pasobj); super.Free(); }', [d.Name]));
    Fjs.WriteLn(Format('public int Size() { return %d; }', [TRecordDef(d).Size]));
  end;

  WrittenItems:=TList.Create;
  try
    RegularClass:=(d.DefType = dtClass) and not TClassDef(d).HasReplacedItems;
    if RegularClass then
      WriteConstructors;
    // Write regular items
    WriteItems(True, False, RegularClass);
    if RegularClass and TClassDef(d).ImplementsReplacedItems then
      // Write implementation wrappers for replaced mehods
      WriteReplacedItems;

    Fjs.DecI;
    Fjs.WriteLn('}');
    Fjs.WriteLn;

    if (d.DefType = dtClass) and (TClassDef(d).HasReplacedItems) then begin
      // Write replaced items
      Fjs.WriteLn(Format('public static class %s extends __%0:s {', [d.AliasName]));
      Fjs.IncI;

      WriteConstructors;
      WriteItems(False, True, True);

      if TClassDef(d).ImplementsReplacedItems then
        // Write implementation wrappers for replaced mehods
        WriteReplacedItems;

      Fjs.DecI;
      Fjs.WriteLn('}');
      Fjs.WriteLn;
    end;
  finally
    WrittenItems.Free;
  end;
end;

procedure TWriter.WriteProc(d: TProcDef; Variable: TVarDef; AParent: TDef);
var
  i, j, ClassIdx: integer;
  s, ss: string;
  err, tf: boolean;
  pi: TProcInfo;
  ci: TClassInfo;
  IsTObject: boolean;
  tempvars: TStringList;
  vd: TVarDef;
  UseTempObjVar: boolean;
  ItemDef: TDef;
begin
  ASSERT(d.DefType = dtProc);
  if d.IsPrivate or not d.IsUsed then
    exit;
  IsTObject:=(d.Parent.DefType = dtClass) and (TClassDef(d.Parent).AncestorClass = nil);
  if (d.ProcType = ptDestructor) and not IsTObject then
    exit;
  if Variable <> nil then
    ItemDef:=Variable
  else
    ItemDef:=d;
  tempvars:=nil;
  pi:=TProcInfo.Create;
  with d do
  try
    pi.Name:=Name;
    s:=GetClassPrefix(d.Parent) + pi.Name;
    pi.JniName:=s;
    pi.JniSignature:=GetProcSignature(d);
    if AParent = nil then begin
      // Checking duplicate name and duplicate params
      ClassIdx:=FClasses.IndexOf(GetJavaClassName(d.Parent, ItemDef));
      if ClassIdx >= 0 then begin
        ci:=TClassInfo(FClasses.Objects[ClassIdx]);
        j:=1;
        ss:=Copy(pi.JniSignature, 1, Pos(')', pi.JniSignature));
        repeat
          err:=False;
          for i:=0 to ci.Funcs.Count - 1 do
            with TProcInfo(ci.Funcs[i]) do
              if CompareText(JniName, pi.JniName) = 0 then begin
                Inc(j);
                pi.JniName:=Format('%s_%d', [s, j]);
                err:=True;
                break;
              end
              else
                if (CompareText(Name, pi.Name) = 0) and (ss = Copy(JniSignature, 1, Pos(')', JniSignature))) then
                  // Duplicate params
                  exit;
        until not err;
      end;

      err:=False;
      if ProcType in [ptFunction, ptConstructor] then
        s:='function'
      else
        s:='procedure';
      s:=s + ' ' + pi.JniName + '(_env: PJNIEnv; _jobj: jobject';

      UseTempObjVar:=(ProcType = ptProcedure) and (Variable <> nil) and (Variable.VarType <> nil) and (Variable.VarType.DefType = dtProcType) and (Variable.Parent.DefType <> dtUnit);

      for j:=0 to Count - 1 do
        with TVarDef(Items[j]) do begin
          s:=s + '; ' + Name + ': ';
          if VarOpt * [voVar, voOut] = [] then
            s:=s + DefToJniType(VarType, err)
          else begin
            s:=s + 'jarray';
            if tempvars = nil then
              tempvars:=TStringList.Create;
            if VarType = nil then
              err:=True
            else
              Tag:=tempvars.AddObject('__tmp_' + Name, d.Items[j]) + 1;
          end;
        end;
      s:=s + ')';

      if ProcType in [ptFunction, ptConstructor] then
        s:=s + ': ' + DefToJniType(ReturnType, err);
      s:=s + '; ' + JniCaliing;
      if err then begin
        d.SetNotUsed;
        s:='// ' + s;
      end;
      Fps.WriteLn;
      Fps.WriteLn(s);
      if err then
        exit;
      if (tempvars <> nil) or UseTempObjVar then begin
        s:='';
        Fps.WriteLn('var');
        Fps.IncI;
        if tempvars <> nil then begin
          for i:=0 to tempvars.Count - 1 do begin
            vd:=TVarDef(tempvars.Objects[i]);
            Fps.WriteLn(Format('%s: %s;', [tempvars[i], vd.VarType.Name]));
            if IsJavaSimpleType(vd.VarType) then begin
              Fps.WriteLn(Format('%s_arr: P%s;', [tempvars[i], DefToJniType(vd.VarType, err)]));
              if s = '' then
                s:='__iscopy: JBoolean;';
            end;
          end;
          if s <> '' then
            Fps.WriteLn(s);
        end;
        if UseTempObjVar then
          Fps.WriteLn('__objvar: ' + d.Parent.Name + ';');
        Fps.DecI;
      end;
      Fps.WriteLn('begin');
      Fps.IncI;
      EHandlerStart;

      tf:=False;
      // Assign var parameter values to local vars
      if tempvars <> nil then begin
        for i:=0 to tempvars.Count - 1 do begin
          vd:=TVarDef(tempvars.Objects[i]);
          Fps.WriteLn(Format('if _env^^.GetArrayLength(_env, %s) <> 1 then _RaiseVarParamException(''%s'');', [vd.Name, vd.Name]));
          if IsJavaSimpleType(vd.VarType) then begin
            Fps.WriteLn(Format('%s_arr:=_env^^.Get%sArrayElements(_env, %s, __iscopy);', [tempvars[i], GetJniFuncType(vd.VarType), vd.Name]));
            Fps.WriteLn(Format('if %s_arr = nil then _RaiseVarParamException(''%s'');', [tempvars[i], vd.Name]));
            s:=tempvars[i] + '_arr^';
            tf:=True;
          end
          else
            s:=Format('_env^^.GetObjectArrayElement(_env, %s, 0)', [vd.Name]);
          if voVar in vd.VarOpt then
            Fps.WriteLn(tempvars[i] + ':=' + JniToPasType(vd.VarType, s, False) + ';');
        end;
      end;

      if tf then begin
        Fps.WriteLn('try');
        Fps.IncI;
      end;

      s:='';
      if Parent.DefType = dtUnit then
        s:=Parent.Name + '.'
      else
        if ProcType = ptConstructor then
          s:=Parent.Parent.Name + '.' + Parent.Name + '.'
        else
          s:=JniToPasType(d.Parent, '_jobj', True) + '.';

      if Variable = nil then begin
        // Regular proc
        s:=s + pi.Name;
        if Count > 0 then begin
          s:=s + '(';
          for j:=0 to Count - 1 do begin
            vd:=TVarDef(Items[j]);
            if vd.Tag <> 0 then
              ss:=tempvars[vd.Tag - 1]
            else begin
              ss:=Items[j].Name;
              ss:=JniToPasType(vd.VarType, ss, False);
            end;
            if j <> 0 then
              s:=s + ', ';
            s:=s + ss;
          end;
          s:=s + ')';
        end;
      end
      else begin
        // Var access
        if UseTempObjVar then begin
          System.Delete(s, Length(s), 1);
          Fps.WriteLn('__objvar:=' + s + ';');
          s:='__objvar.';
        end;
        s:=s + Variable.Name;
        if Variable.IndexType <> nil then begin
          ASSERT(Count >= 1);
          i:=1;
          s:=Format('%s[%s]', [s, JniToPasType(TVarDef(Items[0]).VarType, Items[0].Name, False)]);
        end
        else
          i:=0;
        if ProcType = ptProcedure then begin
          ASSERT(Count = i + 1);
          if Variable.VarType.DefType = dtProcType then begin
            Fps.WriteLn(Format('_RefMethodPtr(_env, TMethod(%s), False);', [s]));
            ss:=Format('_RefMethodPtr(_env, TMethod(%s), True);', [s]);
          end;
          s:=s + ':=' + JniToPasType(TVarDef(Items[i]).VarType, Items[i].Name, False);
        end;
      end;

      if ProcType in [ptFunction, ptConstructor] then
        s:='Result:=' + PasToJniType(ReturnType, s);
      s:=s + ';';
      Fps.WriteLn(s);

      if (Variable <> nil) and UseTempObjVar then
        Fps.WriteLn(ss);

      // Return var/out parameters
      if tempvars <> nil then begin
        for i:=0 to tempvars.Count - 1 do begin
          vd:=TVarDef(tempvars.Objects[i]);
          if IsJavaSimpleType(vd.VarType) then
            Fps.WriteLn(Format('%s_arr^:=%s;', [tempvars[i], PasToJniType(vd.VarType, tempvars[i])]))
          else
            Fps.WriteLn(Format('_env^^.SetObjectArrayElement(_env, %s, 0, %s);', [vd.Name, PasToJniType(vd.VarType, tempvars[i])]));
        end;
      end;

      if IsTObject and ( (ProcType = ptDestructor) or (CompareText(Name, 'Free') = 0) ) then
        Fps.WriteLn(Format('_env^^.SetLongField(_env, _jobj, %s.ObjFieldId, 0);', [GetTypeInfoVar(d.Parent)]));

      if tf then begin
        Fps.WriteLn('finally', -1);

        if tempvars <> nil then begin
          for i:=0 to tempvars.Count - 1 do begin
            vd:=TVarDef(tempvars.Objects[i]);
            if IsJavaSimpleType(vd.VarType) then
              Fps.WriteLn(Format('_env^^.Release%sArrayElements(_env, %s, %s_arr, 0);', [JavaType[TTypeDef(vd.VarType).BasicType], vd.Name, tempvars[i]]));
          end;
        end;

        Fps.DecI;
        Fps.WriteLn('end;');
      end;

      s:='';
      if ProcType in [ptFunction, ptConstructor] then begin
        s:='0';
        if (ReturnType.DefType = dtType) and (TTypeDef(ReturnType).BasicType <= btDouble) then
          s:='0'
        else
          s:=Format('%s(0)', [DefToJniType(ReturnType, err)]);
        s:='Result:=' + s + ';';
      end;
      EHandlerEnd('_env', s);

      Fps.DecI;
      Fps.WriteLn('end;');
      AParent:=d.Parent;
    end
    else
      ClassIdx:=FClasses.IndexOf(GetJavaClassName(AParent, ItemDef));

    if ClassIdx < 0 then begin
      ci:=TClassInfo.Create;
      ci.Def:=AParent;
      s:=GetJavaClassName(AParent, ItemDef);
      ci.IsCommonClass:=s <> AParent.Name;
      ClassIdx:=FClasses.AddObject(s, ci);
    end;
    TClassInfo(FClasses.Objects[ClassIdx]).Funcs.Add(pi);
    pi:=nil;

    // Java part
    s:=GetJavaProcDeclaration(d) + ';';
    if (Parent.DefType = dtUnit) or (ProcType = ptConstructor) then
      s:='static ' + s;

    if Variable = nil then
      Fjs.WriteLn('// ' + GetProcDeclaration(d));
    if poPrivate in ProcOpt then
      ss:='private'
    else
      if poProtected in ProcOpt then
        ss:='protected'
      else
        ss:='public';
    Fjs.WriteLn(ss + ' native ' + s);
  finally
    pi.Free;
    tempvars.Free;
  end;
end;

procedure TWriter.WriteVar(d: TVarDef; AParent: TDef);
var
  pd: TProcDef;
  t: TTypeDef;
  s: string;
begin
  if not d.IsUsed then
    exit;
  if d.VarType <> nil then begin
    case d.DefType of
      dtVar:
        s:='var';
      dtProp:
        s:='property';
      else
        s:='';
    end;
    s:=Trim(s + ' ' + d.Name);
    if d.IndexType <> nil then
      s:=s + '[]';
    Fjs.WriteLn(Format('// %s: %s', [s, d.VarType.Name]));
  end;

  if voRead in d.VarOpt then begin
    pd:=TProcDef.Create(nil, dtProc);
    try
      pd.IsUsed:=True;
      pd.Parent:=d.Parent;
      pd.ProcType:=ptFunction;
      pd.Name:='get' + d.Name;
      pd.ReturnType:=d.VarType;
      if d.IndexType <> nil then
        with TVarDef.Create(pd, dtParam) do begin
          Name:='_Index';
          AliasName:='Index';
          VarType:=d.IndexType;
          VarOpt:=[voRead];
        end;
      WriteProc(pd, d, AParent);
    finally
      pd.Free;
    end;
  end;

  if voWrite in d.VarOpt then begin
    pd:=TProcDef.Create(nil, dtProc);
    try
      pd.IsUsed:=True;
      pd.Parent:=d.Parent;
      pd.ProcType:=ptProcedure;
      pd.Name:='set' + d.Name;
      if d.IndexType <> nil then
        with TVarDef.Create(pd, dtParam) do begin
          Name:='_Index';
          AliasName:='Index';
          VarType:=d.IndexType;
          VarOpt:=[voRead];
        end;
      with TVarDef.Create(pd, dtParam) do begin
        Name:='_Value';
        AliasName:='Value';
        VarType:=d.VarType;
        VarOpt:=[voRead];
      end;
      t:=TTypeDef.Create(nil, dtType);
      try
        t.BasicType:=btVoid;
        pd.ReturnType:=t;
        WriteProc(pd, d, AParent);
      finally
        t.Free;
      end;
    finally
      pd.Free;
    end;
  end;
end;

procedure TWriter.WriteConst(d: TConstDef);
var
  s, v: string;
begin
  if not d.IsUsed then
    exit;
  v:=d.Value;
  if d.VarType = nil then begin
    if Copy(d.Value, 1, 1) = '"' then
      s:='String'
    else
      s:='double';
  end
  else begin
    s:=DefToJavaType(d.VarType);
    if d.VarType.DefType = dtType then
      case TTypeDef(d.VarType).BasicType of
        btLongWord, btInt64:
          v:=v + 'L';
        btBoolean:
          if v = '1' then
            v:='true'
          else
            v:='false';
      end;
  end;
  Fjs.WriteLn(Format('public static final %s %s = %s;', [s, d.Name, v]));
end;

procedure TWriter.WriteEnum(d: TDef);
var
  i: integer;
  s: string;
begin
  if not d.IsUsed then
    exit;

  RegisterPseudoClass(d);

  WriteComment(d, 'enum');
  Fjs.WriteLn(Format('public enum %s {', [d.Name]));
  Fjs.IncI;
  for i:=0 to d.Count - 1 do begin
    s:=Format('%s (%s)', [d[i].Name, TConstDef(d[i]).Value]);
    if i <> d.Count - 1 then
      s:=s + ','
    else
      s:=s + ';';
    Fjs.WriteLn(s);
  end;
  Fjs.WriteLn;
  Fjs.WriteLn('private final int Value;');
  Fjs.WriteLn(Format('%s(int v) { Value=v; }', [d.Name]));
  Fjs.WriteLn('public int Ord() { return Value; }');
  Fjs.DecI;
  Fjs.WriteLn('}');
  Fjs.WriteLn;
end;

procedure TWriter.WriteProcType(d: TProcDef; PreInfo: boolean);

  procedure _AccessSimpleArray(vd: TVarDef; VarIndex: integer; DoSet: boolean);
  begin
    with vd do begin
      Fps.WriteLn(Format('_tmp_%s:=_env^^.Get%sArrayElements(_env, _args[%d].L, PJBoolean(nil)^);', [Name, GetJniFuncType(VarType), VarIndex]));
      Fps.WriteLn(Format('if _tmp_%s <> nil then', [Name]));
      if DoSet then
        Fps.WriteLn(Format('_tmp_%s^:=%s;', [Name, PasToJniType(VarType, Name)]), 1)
      else
        Fps.WriteLn(Format('%s:=%s;', [Name, JniToPasType(VarType, '_tmp_' + Name + '^', False)]), 1);
      Fps.WriteLn(Format('_env^^.Release%sArrayElements(_env, _args[%d].L, _tmp_%s, 0);', [GetJniFuncType(VarType), VarIndex, Name]));
    end;
  end;

var
  vd: TVarDef;
  i: integer;
  s, ss: string;
  err: boolean;
begin
  if not d.IsUsed or not (poMethodPtr in d.ProcOpt) then
    exit;

  if PreInfo then begin
    WriteClassInfoVar(d);

    // Handler proc
    Fps.WriteLn;
    vd:=TVarDef.Create(nil, dtParam);
    try
      vd.Name:='_data';
      vd.VarType:=TTypeDef.Create(nil, dtType);
      with TTypeDef(vd.VarType) do begin
        Name:='pointer';
        BasicType:=btPointer;
      end;
      d.Insert(0, vd);
      Fps.WriteLn(GetProcDeclaration(d, Format('%sHandler', [GetClassPrefix(d)])) + ';');
    finally
      vd.VarType.Free;
      vd.Free;
    end;
    Fps.WriteLn('var');
    Fps.IncI;
    Fps.WriteLn('_env: PJNIEnv;');
    Fps.WriteLn('_mpi: _TMethodPtrInfo;');
    if d.Count > 0 then begin
      Fps.WriteLn(Format('_args: array[0..%d] of jvalue;', [d.Count - 1]));
      for i:=0 to d.Count - 1 do
        with TVarDef(d[i]) do
          if (VarOpt * [voOut, voVar] <> []) and IsJavaSimpleType(VarType) then
            Fps.WriteLn(Format('_tmp_%s: P%s;', [Name, DefToJniType(VarType, err)]));
    end;
    Fps.DecI;
    Fps.WriteLn('begin');
    Fps.IncI;
    Fps.WriteLn('CurJavaVM^^.GetEnv(CurJavaVM, @_env, JNI_VERSION_1_6);');
    Fps.WriteLn('_MethodPointersCS.Enter;');
    Fps.WriteLn('try');
    Fps.WriteLn('_mpi:=_TMethodPtrInfo(_MethodPointers[-integer(ptruint(_data)) - 1]);', 1);
    Fps.WriteLn('finally');
    Fps.WriteLn('_MethodPointersCS.Leave;', 1);
    Fps.WriteLn('end;');

    for i:=0 to d.Count - 1 do
      with TVarDef(d[i]) do begin
        if VarOpt * [voOut, voVar] = [] then begin
          s:='L';
          if VarType.DefType = dtType then
            s:=Copy(JNITypeSig[TTypeDef(VarType).BasicType], 1, 1);
          ss:=PasToJniType(VarType, Name);
        end
        else begin
          s:='L';
          if IsJavaSimpleType(VarType) then
            ss:=Format('_env^^.New%sArray(_env, 1)', [GetJniFuncType(VarType)])
          else begin
            if voVar in VarOpt then
              ss:=PasToJniType(VarType, Name)
            else
              ss:='nil';
            ss:=Format('_env^^.NewObjectArray(_env, 1, %s.ClassRef, %s)', [GetTypeInfoVar(VarType), ss]);
          end;
        end;
        Fps.WriteLn(Format('_args[%d].%s:=%s;', [i, s, ss]));
        if (voVar in VarOpt) and IsJavaSimpleType(VarType) then
          _AccessSimpleArray(TVarDef(d[i]), i, True);
      end;

    if d.Count > 0 then
      s:='@_args'
    else
      s:='nil';
    // Calling Java handler
    s:=Format('_env^^.Call%sMethodA(_env, _mpi.Obj, _mpi.MethodId, %s)', [GetJniFuncType(d.ReturnType), s]);
    if d.ProcType = ptFunction then
      s:=Format('Result:=%s', [JniToPasType(d.ReturnType, s, False)]);
    Fps.WriteLn(s + ';');
    // Processing var/out parameters
    for i:=0 to d.Count - 1 do
      with TVarDef(d[i]) do
        if VarOpt * [voOut, voVar] <> [] then
          if IsJavaSimpleType(VarType) then
            _AccessSimpleArray(TVarDef(d[i]), i, False)
          else begin
            s:=Format('_env^^.GetObjectArrayElement(_env, _args[%d].L, 0)', [i]);
            Fps.WriteLn(Format('%s:=%s;', [Name, JniToPasType(VarType, s, False)]));
          end;

    Fps.DecI;
    Fps.WriteLn('end;');

    // Get handler proc
    Fps.WriteLn;
    Fps.WriteLn(Format('function %sGetHandler(env: PJNIEnv; jobj: jobject; const ci: _TJavaClassInfo): %s.%s;',
                       [GetClassPrefix(d), d.Parent.Name, d.Name]));
    Fps.WriteLn('var mpi: _TMethodPtrInfo;');
    Fps.WriteLn('begin');
    Fps.IncI;
    Fps.WriteLn('Result:=nil;');
    Fps.WriteLn('mpi:=_TMethodPtrInfo(_GetPasObj(env, jobj, ci, False));');
    Fps.WriteLn('if mpi = nil then exit;');
    Fps.WriteLn('if mpi.Index = 0 then');
    Fps.WriteLn('TMethod(Result):=mpi.RealMethod', 1);
    Fps.WriteLn('else');
    Fps.WriteLn('with TMethod(Result) do begin', 1);
    Fps.WriteLn('Data:=pointer(ptruint(-integer(mpi.Index)));', 2);
    Fps.WriteLn(Format('Code:=@%sHandler;', [GetClassPrefix(d)]), 2);
    Fps.WriteLn('end;', 1);
    Fps.DecI;
    Fps.WriteLn('end;');

    exit;
  end;

  err:=False;
  WriteComment(d, 'procedural type');

  RegisterPseudoClass(d);

  Fjs.WriteLn(Format('/* Pascal prototype: %s */', [GetProcDeclaration(d, 'Execute')]));
  Fjs.WriteLn(Format('/* Java prototype: %s */', [GetJavaProcDeclaration(d, 'Execute')]));

  Fjs.WriteLn(Format('public static class %s extends %s.system.MethodPtr {', [d.Name, JavaPackage]));
  Fjs.IncI;
  Fjs.WriteLn(Format('private String HandlerSig = "%s";', [GetProcSignature(d)]));
  Fjs.WriteLn(Format('public %s(Object Obj, String MethodName) { Init(Obj, MethodName, HandlerSig); }', [d.Name]));
  Fjs.WriteLn(Format('public %s() { Init(this, "Execute", HandlerSig); }', [d.Name]));
  Fjs.WriteLn(Format('protected %s throws NoSuchMethodException { throw new NoSuchMethodException(); }', [GetJavaProcDeclaration(d, 'Execute')]));
  Fjs.DecI;
  Fjs.WriteLn('}');
  Fjs.WriteLn;
end;

procedure TWriter.WriteSet(d: TSetDef);
begin
  if not d.IsUsed then
    exit;
  if d.ElType = nil then
    raise Exception.Create('No element type.');

  WriteComment(d, '');
  Fjs.WriteLn(Format('/* set of %s */', [d.ElType.Name]));
  if d.Size > 4 then begin
    Fjs.WriteLn('/* Set size more than 32 bits is not supported */');
    exit;
  end;

  RegisterPseudoClass(d);

  Fjs.WriteLn(Format('public static class %s extends %s.system.Set<%s,%s> {', [d.Name, JavaPackage, d.Name, d.ElType.Name]));
  Fjs.IncI;
  Fjs.WriteLn(Format('protected byte Size() { return %d; }', [d.Size]));
  Fjs.WriteLn(Format('protected int Base() { return %d; }', [d.Base]));
  Fjs.WriteLn(Format('protected int ElMax() { return %d; }', [d.ElMax]));
  Fjs.WriteLn(Format('protected int Ord(%s Element) { return Element.Ord(); }', [d.ElType.Name]));
  Fjs.WriteLn(Format('public %s() { }', [d.Name]));
  Fjs.WriteLn(Format('public %s(%s... Elements) { super(Elements); }', [d.Name, d.ElType.Name]));
  Fjs.WriteLn(Format('public %0:s(%0:s... Elements) { super(Elements); }', [d.Name]));
  Fjs.WriteLn(Format('public static %0:s Exclude(%0:s s1, %0:s s2) { %0:s r = new %0:s(s1); r.Exclude(s2); return r; }', [d.Name]));
  Fjs.WriteLn(Format('public static %0:s Intersect(%0:s s1, %0:s s2) { %0:s r = new %0:s(s1); r.Intersect(s2); return r; }', [d.Name]));
  Fjs.DecI;
  Fjs.WriteLn('}');
  Fjs.WriteLn;
end;

procedure TWriter.WriteUnit(u: TUnitDef);
var
  d: TDef;
  i: integer;
  f: boolean;
begin
  if u.Processed then
    exit;
  u.Processed:=True;

  if not u.IsUsed then
    exit;

  for i:=0 to High(u.UsedUnits) do
    WriteUnit(u.UsedUnits[i]);

  Fps.WriteLn;
  Fps.WriteLn(Format('{ Unit %s }', [u.Name]));

  u.Name:=LowerCase(u.Name);
  Fjs:=TTextOutStream.Create(IncludeTrailingPathDelimiter(FPkgDir) + u.Name + '.java', fmCreate);
  try
    Fjs.WriteLn(Format('package %s;', [JavaPackage]));
    if Length(u.UsedUnits) > 0 then begin
      Fjs.WriteLn;
      f:=False;
      for i:=0 to High(u.UsedUnits) do
        if u.UsedUnits[i].IsUsed then begin
          Fjs.WriteLn(Format('import %s.%s.*;', [JavaPackage, LowerCase(u.UsedUnits[i].Name)]));
          f:=True;
        end;
      if f then
        Fjs.WriteLn('@SuppressWarnings("unused")');
    end;
    Fjs.WriteLn;
    Fjs.WriteLn('public class ' + u.Name + ' {');
    Fjs.IncI;
    if u.Name = 'system' then begin
      Fjs.WriteLn('static private boolean _JniLibLoaded = false;');
      Fjs.WriteLn('public static void InitJni() {');
      Fjs.WriteLn('if (!_JniLibLoaded) {', 1);
      Fjs.WriteLn('_JniLibLoaded=true;', 2);
      Fjs.WriteLn(Format('System.loadLibrary("%s");', [LibName]), 2);
      Fjs.WriteLn('}', 1);
      Fjs.WriteLn('}');

      // Support functions
      Fjs.WriteLn('public native static long AllocMemory(int Size);');
      AddNativeMethod(u, '_AllocMemory', 'AllocMemory', '(I)J');

      // Base object
      Fjs.WriteLn;
      Fjs.WriteLn('public static class PascalObject {');
      Fjs.IncI;
      Fjs.WriteLn(Format('static { %s.system.InitJni(); }', [JavaPackage]));
      Fjs.WriteLn('protected long _pasobj = 0;');
      Fjs.DecI;
      Fjs.WriteLn('}');
      Fjs.WriteLn;
      Fjs.WriteLn('public static long Pointer(PascalObject obj) { return obj._pasobj; }');

      // Record
      Fjs.WriteLn;
      Fjs.WriteLn('public static class Record extends PascalObject {');
      Fjs.IncI;
      Fjs.WriteLn('protected void finalize() { Free(); }');
      Fjs.WriteLn('public Record() { _pasobj = AllocMemory(Size()); }');
      Fjs.WriteLn('public void Free() { _pasobj = 0; }');
      Fjs.WriteLn('public int Size() { return 0; }');
      Fjs.DecI;
      Fjs.WriteLn('}');

      // Method pointer base class
      d:=TClassDef.Create(FThisUnit, dtClass);
      d.Name:='_TMethodPtrInfo';
      d.AliasName:='MethodPtr';
      WriteClassInfoVar(d);

      Fps.WriteLn;
      Fps.WriteLn('procedure _TMethodPtrInfo_Init(env: PJNIEnv; _self, JavaObj: JObject; AMethodName, AMethodSig: jstring);' + JniCaliing);
      Fps.WriteLn('var mpi: _TMethodPtrInfo;');
      Fps.WriteLn('begin');
      Fps.IncI;
      EHandlerStart;
      Fps.WriteLn('mpi:=_TMethodPtrInfo.Create(env, JavaObj, ansistring(_StringFromJString(env, AMethodName)), ansistring(_StringFromJString(env, AMethodSig)));');
      Fps.WriteLn(Format('env^^.SetLongField(env, _self, %s.ObjFieldId, Int64(ptruint(mpi)));', [GetTypeInfoVar(d)]));
      EHandlerEnd('env');
      Fps.DecI;
      Fps.WriteLn('end;');

      AddNativeMethod(d, '_TMethodPtrInfo_Init', 'Init', Format('(Ljava/lang/Object;%s%s)V', [JNITypeSig[btString], JNITypeSig[btString]]));

      Fps.WriteLn;
      Fps.WriteLn('procedure _TMethodPtrInfo_Release(env: PJNIEnv; _self: JObject);' + JniCaliing);
      Fps.WriteLn('begin');
      Fps.IncI;
      EHandlerStart;
      Fps.WriteLn(Format('_TMethodPtrInfo(_GetPasObj(env, _self, %s, True)).Release(env);', [GetTypeInfoVar(d)]));
      EHandlerEnd('env');
      Fps.DecI;
      Fps.WriteLn('end;');

      AddNativeMethod(d, '_TMethodPtrInfo_Release', 'Release', '()V');

      Fjs.WriteLn;
      Fjs.WriteLn('public static class MethodPtr extends PascalObject {');
      Fjs.IncI;

      Fjs.WriteLn('private native void Release();');
      Fjs.WriteLn('protected void finalize() { if (_pasobj != 0) Release(); }');
      Fjs.WriteLn('protected native void Init(Object Obj, String MethodName, String MethodSignature);');
      Fjs.DecI;
      Fjs.WriteLn('}');
      Fjs.WriteLn;

      // Set base class
      Fjs.WriteLn('public static class Set<TS extends Set<?,?>,TE> {');
      Fjs.IncI;
      Fjs.WriteLn('protected int Value = 0;');
      Fjs.WriteLn('protected byte Size() { return 0; }');
      Fjs.WriteLn('protected int Base() { return 0; }');
      Fjs.WriteLn('protected int ElMax() { return 0; }');
      Fjs.WriteLn('protected int Ord(TE Element) { return 0; }');
      Fjs.WriteLn('protected int GetMask(TE Element) {');
      Fjs.IncI;
      Fjs.WriteLn('return 1 << (Ord(Element) - Base());');
      Fjs.DecI;
      Fjs.WriteLn('}');
      Fjs.WriteLn('public Set() { }');
      Fjs.WriteLn('public Set(TE... Elements) { Include(Elements); }');
      Fjs.WriteLn('public Set(TS... Elements) { for (TS e : Elements) Include(e); }');
      Fjs.WriteLn('public void Include(TE... Elements) { for (TE e: Elements) Value = Value | GetMask(e); }');
      Fjs.WriteLn('public void Include(TS s) { Value=Value | s.Value; }');
      Fjs.WriteLn('public void Exclude(TE... Elements) { for (TE e: Elements) Value = Value & ~GetMask(e); }');
      Fjs.WriteLn('public void Exclude(TS s) { Value=Value & ~s.Value; }');
      Fjs.WriteLn('public void Assign(TS s) { Value=s.Value; }');
      Fjs.WriteLn('public void Intersect(TS s) { Value=Value & s.Value; }');
      Fjs.WriteLn('public boolean Compare(TS s) { return Value == s.Value; }');
      Fjs.WriteLn('public boolean Has(TE Element) { return (Value & GetMask(Element)) != 0; }');
      Fjs.DecI;
      Fjs.WriteLn('}');
      Fjs.WriteLn;
    end;
    Fjs.WriteLn(Format('static { %s.system.InitJni(); }', [JavaPackage]));
    Fjs.WriteLn;

    // First pass
    for i:=0 to u.Count - 1 do begin
      d:=u[i];
      if not d.IsUsed then
        continue;
      case d.DefType of
        dtSet, dtEnum:
          WriteClassInfoVar(d);
        dtClass, dtRecord:
          WriteClass(d, True);
        dtProcType:
          WriteProcType(TProcDef(d), True);
      end;
    end;

    // Second pass
    for i:=0 to u.Count - 1 do begin
      d:=u[i];
      if not d.IsUsed then
        continue;
      case d.DefType of
        dtClass, dtRecord:
          WriteClass(d, False);
        dtProc:
          WriteProc(TProcDef(d));
        dtVar, dtProp:
          WriteVar(TVarDef(d));
        dtEnum:
          WriteEnum(d);
        dtProcType:
          WriteProcType(TProcDef(d), False);
        dtSet:
          WriteSet(TSetDef(d));
        dtConst:
          WriteConst(TConstDef(d));
      end;
    end;

    Fjs.DecI;
    Fjs.WriteLn('}');
  finally
    Fjs.Free;
  end;
end;

procedure TWriter.WriteOnLoad;
var
  i, j: integer;
  ci: TClassInfo;
  s, ss, fn: string;
  d: TTypeDef;
begin
  if FClasses.Count = 0 then
    exit;
  Fps.WriteLn;
  Fps.WriteLn('function JNI_OnLoad(vm: PJavaVM; reserved: pointer): jint;' + JniCaliing);

  Fps.WriteLn('const');
  for i:=0 to FClasses.Count - 1 do begin
    ci:=TClassInfo(FClasses.Objects[i]);
    if ci.Funcs.Count = 0 then
      continue;
    Fps.WriteLn(Format('  _%sNativeMethods: array[0..%d] of JNINativeMethod = (', [GetClassPrefix(ci.Def, FClasses[i]), ci.Funcs.Count - 1]));
    for j:=0 to ci.Funcs.Count - 1 do begin
      with TProcInfo(ci.Funcs[j]) do
        Fps.Write(Format('    (name: ''%s''; signature: ''%s''; fnPtr: @%s)', [Name, JniSignature, JniName]));
      if j < ci.Funcs.Count - 1 then
        Fps.Write(',');
      Fps.WriteLn;
    end;
    Fps.WriteLn('  );');
  end;

  Fps.WriteLn;
  Fps.WriteLn('var');
  Fps.IncI;
  Fps.WriteLn('env: PJNIEnv;');
  Fps.WriteLn;
  Fps.WriteLn('function _Reg(ClassName: PAnsiChar; Methods: PJNINativeMethod; Count: integer; ci: _PJavaClassInfo; const FieldName: ansistring = ''_pasobj''; const FieldSig: ansistring = ''J''): boolean;');
  Fps.WriteLn('var');
  Fps.WriteLn('c: jclass;', 1);
  Fps.WriteLn('begin');
  Fps.IncI;
  Fps.WriteLn('Result:=False;');
  Fps.WriteLn('c:=env^^.FindClass(env, ClassName);');
  Fps.WriteLn('if c = nil then exit;');
  Fps.WriteLn('Result:=(Count = 0) or (env^^.RegisterNatives(env, c, Methods, Count) = 0);');
  Fps.WriteLn('if Result and (ci <> nil) then begin');
  Fps.IncI;
  Fps.WriteLn('ci^.ClassRef:=env^^.NewGlobalRef(env, c);');
  Fps.WriteLn('Result:=ci^.ClassRef <> nil;');
  Fps.WriteLn('if Result and (FieldName <> '''') then begin');
  Fps.WriteLn('ci^.ObjFieldId:=env^^.GetFieldID(env, ci^.ClassRef, PAnsiChar(FieldName), PAnsiChar(FieldSig));', 1);
  Fps.WriteLn('Result:=ci^.ObjFieldId <> nil;', 1);
  Fps.WriteLn('end;');
  Fps.DecI;
  Fps.WriteLn('end;');
  Fps.DecI;
  Fps.WriteLn('end;');
  Fps.WriteLn;
  Fps.WriteLn('begin', -1);
  Fps.WriteLn('Result:=JNI_ERR;');
  Fps.WriteLn('if vm^^.GetEnv(vm, @env, JNI_VERSION_1_6) <> JNI_OK then exit;');
  Fps.WriteLn('CurJavaVM:=vm;');

  d:=TTypeDef.Create(nil, dtType);
  try
    d.BasicType:=btString;
    s:=JNITypeSig[d.BasicType];
    s:=Copy(s, 2, Length(s) - 2);
    Fps.WriteLn(Format('if not _Reg(''%s'', nil, 0, @%s, '''', '''') then exit;',
                       [s, GetTypeInfoVar(d)]));
  finally
    d.Free;
  end;

  for i:=0 to FClasses.Count - 1 do begin
    ci:=TClassInfo(FClasses.Objects[i]);
    s:=GetTypeInfoVar(ci.Def);
    if (s = '') or (ci.IsCommonClass) then
      s:='nil'
    else
      s:='@' + s;
    if ci.Funcs.Count = 0 then
      ss:='nil'
    else
      ss:=Format('@_%sNativeMethods', [GetClassPrefix(ci.Def, FClasses[i])]);
    fn:='';
    if ci.Def <> nil then
      if ci.Def.DefType in [dtSet, dtEnum] then
        fn:=', ''Value'', ''I''';
    Fps.WriteLn(Format('if not _Reg(''%s'', %s, %d, %s%s) then exit;',
                       [GetJavaClassPath(ci.Def, FClasses[i]), ss, ci.Funcs.Count, s, fn]));
  end;

  Fps.WriteLn('Result:=JNI_VERSION_1_6;');
  Fps.DecI;
  Fps.WriteLn('end;');
  Fps.WriteLn;
  Fps.WriteLn('exports JNI_OnLoad;');
end;

function TWriter.JniToPasType(d: TDef; const v: string; CheckNil: boolean): string;
var
  n: string;
begin
  Result:=v;
  if d = nil then
    exit;
  case d.DefType of
    dtType:
      with TTypeDef(d) do
        case BasicType of
          btString, btWideString:
            begin
              Result:=Format('_StringFromJString(_env, %s)', [Result]);
              if BasicType <> btWideString then
                Result:=Format('%s(%s)', [d.Name, Result]);
            end;
          btBoolean:
            Result:=Format('LongBool(%s)', [Result]);
          btChar:
            Result:=Format('char(widechar(%s))', [Result]);
          btWideChar:
            Result:=Format('widechar(%s)', [Result]);
          btPointer:
            Result:=Format('pointer(ptruint(%s))', [Result]);
          btGuid:
            Result:=Format('StringToGUID(ansistring(_StringFromJString(_env, %s)))', [Result]);
          else
            Result:=Format('%s(%s)', [d.Name, Result]);
        end;
    dtClass:
      begin
        if CheckNil then
          n:='True'
        else
          n:='False';
        Result:=Format('%s.%s(_GetPasObj(_env, %s, %s, %s))', [d.Parent.Name, d.Name, Result, GetTypeInfoVar(d), n]);
      end;
    dtRecord:
      Result:=Format('%s.%s(_GetPasObj(_env, %s, %s, True)^)', [d.Parent.Name, d.Name, Result, GetTypeInfoVar(d)]);
    dtProcType:
      Result:=Format('%sGetHandler(_env, %s, %s)', [GetClassPrefix(d), Result, GetTypeInfoVar(d)]);
    dtEnum:
      Result:=Format('%s.%s(_GetIntObjValue(_env, %s, %s))', [d.Parent.Name, d.Name, Result, GetTypeInfoVar(d)]);
    dtSet:
      Result:=Format('%s.%s(%s(_GetIntObjValue(_env, %s, %s)))', [d.Parent.Name, d.Name, GetPasIntType(TSetDef(d).Size), Result, GetTypeInfoVar(d)]);
  end;
end;

function TWriter.PasToJniType(d: TDef; const v: string): string;
begin
  Result:=v;
  if d = nil then
    exit;
  case d.DefType of
    dtType:
      with TTypeDef(d) do
        case BasicType of
          btString, btWideString:
            Result:=Format('_StringToJString(_env, _JNIString(%s))', [Result]);
          btBoolean:
            Result:=Format('jboolean(LongBool(%s))', [Result]);
          btChar:
            Result:=Format('jchar(widechar(%s))', [Result]);
          btWideChar:
            Result:=Format('jchar(%s)', [Result]);
          btEnum:
            Result:=Format('jint(%s)', [Result]);
          btPointer:
            Result:=Format('ptruint(pointer(%s))', [Result]);
          btGuid:
            Result:=Format('_StringToJString(_env, _JNIString(GUIDToString(%s)))', [Result]);
        end;
    dtClass:
      Result:=Format('_CreateJavaObj(_env, %s, %s)', [Result, GetTypeInfoVar(d)]);
    dtRecord:
      Result:=Format('_%s_CreateObj(_env, %s)', [GetClassPrefix(d), Result]);
    dtProcType:
      Result:=Format('_CreateMethodPtrObject(_env, TMethod(%s), %s)', [Result, GetTypeInfoVar(d)]);
    dtEnum:
      Result:=Format('_CreateIntObj(_env, longint(%s), %s)', [Result, GetTypeInfoVar(d)]);
    dtSet:
      Result:=Format('_CreateIntObj(_env, %s(%s), %s)', [GetPasIntType(TSetDef(d).Size), Result, GetTypeInfoVar(d)]);
  end;
end;

function TWriter.GetTypeInfoVar(ClassDef: TDef): string;
begin
  if ClassDef.DefType = dtUnit then
    Result:=''
  else
    if ClassDef.DefType = dtType then
      Result:='_Java_' + JavaType[TTypeDef(ClassDef).BasicType] + '_Info'
    else
      Result:='_JNI_' + ClassDef.Parent.Name + '_' + ClassDef.Name + '_Info';
end;

function TWriter.GetClassPrefix(ClassDef: TDef; const AClassName: string): string;
begin
  if AClassName = '' then
    Result:=ClassDef.Name
  else
    Result:=AClassName;
  Result:=Result + '_';
  if ClassDef.DefType <> dtUnit then
    Result:=ClassDef.Parent.Name + '_' + Result;
  Result:='JNI_' + Result;
end;

function TWriter.IsJavaSimpleType(d: TDef): boolean;
begin
  Result:=(d <> nil) and (d.DefType = dtType) and (Length(JNITypeSig[TTypeDef(d).BasicType]) = 1);
end;

function TWriter.GetProcDeclaration(d: TProcDef; const ProcName: string): string;
var
  s, ss: string;
  j: integer;
begin
  with d do begin
    if Count > 0 then
      s:='('
    else
      s:='';
    for j:=0 to Count - 1 do
      with TVarDef(Items[j]) do begin
        if j > 0 then
          s:=s + '; ';
        if voVar in VarOpt then
          s:=s + 'var '
        else
        if voOut in VarOpt then
          s:=s + 'out '
        else
        if voConst in VarOpt then
          s:=s + 'const ';
        s:=s + Name + ': ' + VarType.Name;
      end;

    if Count > 0 then
      s:=s + ')';
    case ProcType of
      ptConstructor:
        ss:='constructor';
      ptDestructor:
        ss:='destructor';
      ptProcedure:
        ss:='procedure';
      ptFunction:
        ss:='function';
      else
        ss:='';
    end;
    if ProcType in [ptConstructor, ptFunction] then
      s:=s + ': ' + ReturnType.Name;
    ss:=ss + ' ';
    if ProcName <> '' then
      ss:=ss + ProcName
    else
      ss:=ss + Name;
    Result:=ss + s;
  end;
end;

function TWriter.GetJavaProcDeclaration(d: TProcDef; const ProcName: string): string;
var
  s: string;
  j: integer;
begin
  with d do begin
    if ProcName <> '' then
      s:=ProcName
    else
      s:=AliasName;
    s:=DefToJavaType(ReturnType) + ' ' + s + '(';
    for j:=0 to Count - 1 do
      with TVarDef(Items[j]) do begin
        if j > 0 then
          s:=s + ', ';
        s:=s + DefToJavaType(VarType);
        if VarOpt * [voVar, voOut] <> [] then
          s:=s + '[]';
        s:=s + ' ' + AliasName;
      end;
    s:=s + ')';
  end;
  Result:=s;
end;

function TWriter.GetJniFuncType(d: TDef): string;
begin
  if IsJavaSimpleType(d) then begin
    Result:=JavaType[TTypeDef(d).BasicType];
    Result[1]:=UpCase(Result[1]);
  end
  else
    Result:='Object';
end;

function TWriter.GetJavaClassName(cls: TDef; it: TDef): string;
begin
  Result:=cls.AliasName;
  if (cls.DefType <> dtClass) or ((it <> nil) and not (it.DefType in ReplDefs)) then
    exit;
  with TClassDef(cls) do begin
    if not (HasReplacedItems or ImplementsReplacedItems) then
      exit;
    if ImplementsReplacedItems and not HasReplacedItems then
      exit;
    if it <> nil then
      with TReplDef(it) do begin
        if (it.DefType = dtProc) and (TProcDef(it).ProcType = ptConstructor) then
          exit;
        if IsReplaced or IsReplImpl then
          exit;
      end;
  end;
  Result:='__' + Result;
end;

procedure TWriter.RegisterPseudoClass(d: TDef);
var
  ci: TClassInfo;
begin
  if FClasses.IndexOf(d.Name) < 0 then begin
    ci:=TClassInfo.Create;
    ci.Def:=d;
    FClasses.AddObject(d.Name, ci);
  end;
end;

function TWriter.GetPasIntType(Size: integer): string;
begin
  case Size of
    1: Result:='byte';
    2: Result:='word';
    else
       Result:='cardinal';
  end;
end;

function TWriter.AddCustomProc(ParentDef: TDef; const JniName, Name: string; RetType: TBasicType; const Params: array of TBasicType): TProcDef;
var
  i: integer;
  vd: TVarDef;
begin
  Result:=TProcDef.Create(ParentDef, dtProc);
  Result.Name:=JniName;
  Result.AliasName:=Name;
  if RetType = btVoid then
    Result.ProcType:=ptProcedure
  else
    Result.ProcType:=ptFunction;
  for i:=0 to High(Params) do begin
    vd:=TVarDef.Create(Result, dtParam);
    vd.Name:=Format('p%d', [i + 1]);
    vd.VarType:=TTypeDef.Create(vd, dtType);
    TTypeDef(vd.VarType).BasicType:=Params[i];
  end;
  Result.ReturnType:=TTypeDef.Create(ParentDef, dtType);
  TTypeDef(Result.ReturnType).BasicType:=RetType;
end;

procedure TWriter.AddNativeMethod(ParentDef: TDef; const JniName, Name, Signature: string);
var
  i: integer;
  ci: TClassInfo;
  pi: TProcInfo;
begin
  pi:=TProcInfo.Create;
  pi.Name:=Name;
  pi.JniName:=JniName;
  pi.JniSignature:=Signature;
  i:=FClasses.IndexOf(ParentDef.AliasName);
  if i < 0 then begin
    ci:=TClassInfo.Create;
    ci.Def:=ParentDef;
    i:=FClasses.AddObject(ParentDef.AliasName, ci);
  end;
  TClassInfo(FClasses.Objects[i]).Funcs.Add(pi);
end;

function TWriter.GetProcSignature(d: TProcDef): string;
var
  j: integer;
begin
  Result:='(';
  for j:=0 to d.Count - 1 do
    with TVarDef(d[j]) do begin
      if VarOpt * [voVar, voOut] <> [] then
        Result:=Result + '[';
      Result:=Result + DefToJniSig(VarType);
    end;
  Result:=Result + ')' + DefToJniSig(d.ReturnType);
end;

procedure TWriter.EHandlerStart;
begin
  Fps.WriteLn('try');
  Fps.IncI;
end;

procedure TWriter.EHandlerEnd(const EnvVarName: string; const ExtraCode: string);
begin
  Fps.WriteLn('except', -1);
  Fps.WriteLn(Format('_HandleJNIException(%s);', [EnvVarName]));
  if ExtraCode <> '' then
    Fps.WriteLn(ExtraCode);
  Fps.DecI;
  Fps.WriteLn('end;');
end;

procedure TWriter.WriteClassInfoVar(d: TDef);
begin
  Fps.WriteLn;
  Fps.WriteLn(Format('var %s: _TJavaClassInfo;', [GetTypeInfoVar(d)]));
end;

procedure TWriter.WriteComment(d: TDef; const AType: string);
begin
  Fps.WriteLn;
  Fps.WriteLn(Format('{ %s }', [d.Name]));

  Fjs.WriteLn(Format('/* %s %s */', [AType, d.Name]));
{$ifdef DEBUG}
  Fjs.WriteLn(Format('/* Ref count: %d */', [d.RefCnt]));
{$endif}
end;

{
procedure TWriter.AddCustomProc(ParentDef: TDef; const JniName, Name: string; RetType: TBasicType; const Params: array of TBasicType);
var
  i: integer;
  ci: TClassInfo;
  pi: TProcInfo;
begin
  pi:=TProcInfo.Create;
  pi.Name:=Name;
  pi.JniName:=JniName;
  pi.JniSignature:='(';
  for i:=0 to High(Params) do
    pi.JniSignature:=pi.JniSignature + JNITypeSig[Params[i]];
  pi.JniSignature:=pi.JniSignature + ')';
  pi.JniSignature:=pi.JniSignature + JNITypeSig[RetType];

  i:=FClasses.IndexOf(ParentDef.Name);
  if i < 0 then begin
    ci:=TClassInfo.Create;
    ci.Def:=ParentDef;
    i:=FClasses.AddObject(ParentDef.Name, ci);
  end;
  TClassInfo(FClasses.Objects[i]).Funcs.Add(pi);
end;
}
constructor TWriter.Create;
var
  i: integer;
begin
  Units:=TStringList.Create;
  FClasses:=TStringList.Create;
  FClasses.Sorted:=True;
  JavaPackage:='pas';
  IncludeList:=TStringList.Create;
  IncludeList.Duplicates:=dupIgnore;
  ExcludeList:=TStringList.Create;
  ExcludeList.Duplicates:=dupIgnore;

  for i:=Low(ExcludeStd) to High(ExcludeStd) do
    ExcludeList.Add(ExcludeStd[i]);
  for i:=Low(ExcludeDelphi7) to High(ExcludeDelphi7) do
    ExcludeList.Add(ExcludeDelphi7[i]);

  FThisUnit:=TUnitDef.Create(nil, dtUnit);
end;

destructor TWriter.Destroy;
var
  i: integer;
begin
  for i:=0 to FClasses.Count - 1 do
    FClasses.Objects[i].Free;
  FClasses.Free;
  Units.Free;
  IncludeList.Free;
  ExcludeList.Free;
  FThisUnit.Free;
  inherited Destroy;
end;

procedure TWriter.ProcessUnits;
var
  p: TPPUParser;
  i: integer;
  s, ss: string;
  d: TDef;
begin
  if Units.Count = 0 then
    raise Exception.Create('No unit name specified.');
  if (OutPath <> '') and not DirectoryExists(OutPath) then
    raise Exception.CreateFmt('Output path "%s" does not exist.', [OutPath]);
  if (JavaOutPath <> '') and not DirectoryExists(JavaOutPath) then
    raise Exception.CreateFmt('Output path "%s" does not exist.', [JavaOutPath]);
  if LibName = '' then
    LibName:=AnsiLowerCase(ChangeFileExt(Units[0], '')) + 'jni';

  for i:=0 to IncludeList.Count - 1 do
    IncludeList[i]:=Trim(IncludeList[i]);
  IncludeList.Sorted:=True;
  for i:=0 to ExcludeList.Count - 1 do
    ExcludeList[i]:=Trim(ExcludeList[i]);
  ExcludeList.Sorted:=True;

  FThisUnit.Name:=LibName;
  FThisUnit.AliasName:='system';

  p:=TPPUParser.Create(SearchPath);
  try
    p.OnCheckItem:=@DoCheckItem;
    for i:=0 to Units.Count - 1 do
      IncludeList.Add(ChangeFileExt(ExtractFileName(Units[i]), ''));
    for i:=0 to Units.Count - 1 do
      p.Parse(ChangeFileExt(ExtractFileName(Units[i]), ''));

    if OutPath <> '' then
      OutPath:=IncludeTrailingPathDelimiter(OutPath);
    if JavaOutPath <> '' then
      JavaOutPath:=IncludeTrailingPathDelimiter(JavaOutPath);

    FPkgDir:=JavaOutPath + StringReplace(JavaPackage, '.', DirectorySeparator, [rfReplaceAll]);
    ForceDirectories(FPkgDir);
    Fps:=TTextOutStream.Create(OutPath + LibName + '.pas', fmCreate);

    Fps.WriteLn('library '+ LibName + ';');
    Fps.WriteLn('{$ifdef fpc} {$mode objfpc} {$H+} {$endif}');

    Fps.WriteLn;
    Fps.WriteLn('uses');
    Fps.WriteLn('{$ifndef FPC} Windows, {$endif} {$ifdef unix} cthreads, {$endif} SysUtils, SyncObjs,', 1);
    s:='';
    for i:=0 to p.Units.Count - 1 do begin
      ProcessRules(p.Units[i]);
      ss:=LowerCase(p.Units[i].Name);
      if (ss ='system') or (ss = 'objpas') or (ss = 'sysutils') or (ss = 'syncobjs') or (ss = 'jni') then
        continue;
      if s <> '' then
        s:=s + ', ';
      s:=s + p.Units[i].Name;
    end;
    Fps.WriteLn(s + ', jni;', 1);

    // Types
    Fps.WriteLn;
    Fps.WriteLn('type');
    Fps.IncI;
    Fps.WriteLn('_JNIString = {$ifdef FPC} unicodestring {$else} widestring {$endif};');
    Fps.WriteLn('{$ifndef FPC} ptruint = cardinal; {$endif}');
    Fps.WriteLn;
    Fps.WriteLn('_TJavaClassInfo = record');
    Fps.WriteLn('ClassRef: JClass;', 1);
    Fps.WriteLn('ObjFieldId: JFieldId;', 1);
    Fps.WriteLn('end;');
    Fps.WriteLn('_PJavaClassInfo = ^_TJavaClassInfo;');
    Fps.DecI;

    Fps.WriteLn;
    d:=TtypeDef.Create(nil, dtType);
    TtypeDef(d).BasicType:=btString;
    Fps.WriteLn(Format('var %s: _TJavaClassInfo;', [GetTypeInfoVar(d)]));
    d.Free;

    // Support functions
    Fps.WriteLn;
    Fps.WriteLn('function _StringFromJString(env: PJNIEnv; s: jstring): _JNIString;');
    Fps.WriteLn('var');
    Fps.WriteLn('p: PJChar;', 1);
    Fps.WriteLn('c: JBoolean;', 1);
    Fps.WriteLn('begin');
    Fps.WriteLn('if s = nil then begin', 1);
    Fps.WriteLn('Result:='''';', 2);
    Fps.WriteLn('exit;', 2);
    Fps.WriteLn('end;', 1);
    Fps.WriteLn('p:=env^^.GetStringChars(env, s, c);', 1);
    Fps.WriteLn('SetString(Result, PWideChar(p), env^^.GetStringLength(env, s));', 1);
    Fps.WriteLn('env^^.ReleaseStringChars(env, s, p);', 1);
    Fps.WriteLn('end;');

    Fps.WriteLn;
    Fps.WriteLn('function _StringToJString(env: PJNIEnv; const s: _JNIString): jstring;');
    Fps.WriteLn('begin');
    Fps.WriteLn('Result:=env^^.NewString(env, PJChar(PWideChar(s)), Length(s));', 1);
    Fps.WriteLn('end;');

    Fps.WriteLn;
    Fps.WriteLn('function _CreateJavaObj(env: PJNIEnv; PasObj: pointer; const ci: _TJavaClassInfo): jobject;');
    Fps.WriteLn('begin');
    Fps.IncI;
    Fps.WriteLn('Result:=nil;');
    Fps.WriteLn('Result:=env^^.AllocObject(env, ci.ClassRef);');
    Fps.WriteLn('if Result = nil then exit;');
    Fps.WriteLn('env^^.SetLongField(env, Result, ci.ObjFieldId, Int64(ptruint(PasObj)));');
    Fps.DecI;
    Fps.WriteLn('end;');

    Fps.WriteLn;
    Fps.WriteLn('function _GetPasObj(env: PJNIEnv; jobj: jobject; const ci: _TJavaClassInfo; CheckNil: boolean): pointer;');
    Fps.WriteLn('var pasobj: jlong;');
    Fps.WriteLn('begin');
    Fps.IncI;
    Fps.WriteLn('if jobj <> nil then');
    Fps.WriteLn('pasobj:=env^^.GetLongField(env, jobj, ci.ObjFieldId)', 1);
    Fps.WriteLn('else');
    Fps.WriteLn('pasobj:=0;', 1);
    Fps.WriteLn('if CheckNil and (pasobj = 0) then');
    Fps.WriteLn('raise Exception.Create(''Attempt to access a released Pascal object.'');', 1);
    Fps.WriteLn('Result:=pointer(ptruint(pasobj));');
    Fps.DecI;
    Fps.WriteLn('end;');

    Fps.WriteLn;
    Fps.WriteLn('procedure _HandleJNIException(env: PJNIEnv);');
    Fps.WriteLn('begin');
    Fps.WriteLn('env^^.ThrowNew(env, env^^.FindClass(env, ''java/lang/Exception''), PAnsiChar(Utf8Encode(Exception(ExceptObject).Message)));', 1);
    Fps.WriteLn('end;');

    Fps.WriteLn;
    Fps.WriteLn('procedure _RaiseVarParamException(const VarName: string);');
    Fps.WriteLn('begin');
    Fps.WriteLn('raise Exception.CreateFmt(''An array with only single element must be passed as parameter "%s".'', [VarName]);', 1);
    Fps.WriteLn('end;');

    Fps.WriteLn;
    Fps.WriteLn('function _AllocMemory(env: PJNIEnv; jobj: jobject; size: jint): jlong;');
    Fps.WriteLn('var p: pointer;');
    Fps.WriteLn('begin');
    Fps.WriteLn('GetMem(p, size);', 1);
    Fps.WriteLn('FillChar(p^, size, 0);', 1);
    Fps.WriteLn('Result:=ptruint(p);', 1);
    Fps.WriteLn('end;');

    // Method pointer support
    Fps.WriteLn;
    Fps.WriteLn('type');
    Fps.IncI;
    Fps.WriteLn('_TMethodPtrInfo = class');
    Fps.IncI;
    Fps.WriteLn('Obj: JObject;');
    Fps.WriteLn('MethodId: JMethodID;');
    Fps.WriteLn('Index, RefCnt: integer;');
    Fps.WriteLn('RealMethod: TMethod;');
    Fps.WriteLn('constructor Create(env: PJNIEnv; JavaObj: JObject; const AMethodName, AMethodSig: ansistring);');
    Fps.WriteLn('procedure Release(env: PJNIEnv);');
    Fps.DecI;
    Fps.WriteLn('end;');
    Fps.DecI;
    Fps.WriteLn;
    Fps.WriteLn('var _MethodPointers: array of _TMethodPtrInfo;');
    Fps.WriteLn('var _MethodPointersCS: TCriticalSection;');
    Fps.WriteLn;

    Fps.WriteLn('constructor _TMethodPtrInfo.Create(env: PJNIEnv; JavaObj: JObject; const AMethodName, AMethodSig: ansistring);');
    Fps.WriteLn('var c: JClass;');
    Fps.WriteLn('begin');
    Fps.IncI;
    Fps.WriteLn('RefCnt:=1;');
    Fps.WriteLn('if (JavaObj = nil) or (AMethodName = '''') then exit;');
    Fps.WriteLn('c:=env^^.GetObjectClass(env, JavaObj);');
    Fps.WriteLn('if c = nil then exit;');
    Fps.WriteLn('MethodId:=env^^.GetMethodID(env, c, PAnsiChar(AMethodName), PAnsiChar(AMethodSig));');
    Fps.WriteLn('if MethodId = nil then raise Exception.CreateFmt(''Method "%s" does not exist or has wrong parameters.'', [AMethodName]);');
    Fps.WriteLn('Obj:=env^^.NewGlobalRef(env, JavaObj);');
    Fps.WriteLn('_MethodPointersCS.Enter;');
    Fps.WriteLn('try');
    Fps.IncI;
    Fps.WriteLn('Index:=Length(_MethodPointers) + 1;');
    Fps.WriteLn(Format('if Index > %d then raise Exception.Create(''Too many method pointers.'');', [MaxMethodPointers]));
    Fps.WriteLn('SetLength(_MethodPointers, Index);');
    Fps.WriteLn('_MethodPointers[Index - 1]:=Self;');
    Fps.WriteLn('finally', -1);
    Fps.WriteLn('_MethodPointersCS.Leave;');
    Fps.DecI;
    Fps.WriteLn('end;');
    Fps.DecI;
    Fps.WriteLn('end;');

    Fps.WriteLn;
    Fps.WriteLn('procedure _TMethodPtrInfo.Release(env: PJNIEnv);');
    Fps.WriteLn('var i: integer;');
    Fps.WriteLn('begin');
    Fps.IncI;
    Fps.WriteLn('i:=InterlockedDecrement(RefCnt);');
    Fps.WriteLn('if i <> 0 then exit;');
    Fps.WriteLn('if Index > 0 then begin');
    Fps.IncI;
    Fps.WriteLn('_MethodPointersCS.Enter;');
    Fps.WriteLn('try');
    Fps.IncI;
    Fps.WriteLn('Dec(Index);');
    Fps.WriteLn('_MethodPointers[Index]:=nil;');
    Fps.WriteLn('Index:=Length(_MethodPointers);');
    Fps.WriteLn('while (Index > 0) and (_MethodPointers[Index] = nil) do Dec(Index);');
    Fps.WriteLn('SetLength(_MethodPointers, Index + 1);');
    Fps.WriteLn('finally', -1);
    Fps.WriteLn('_MethodPointersCS.Leave;');
    Fps.DecI;
    Fps.WriteLn('end;');
    Fps.WriteLn('env^^.DeleteGlobalRef(env, Obj);');
    Fps.DecI;
    Fps.WriteLn('end;');
    Fps.WriteLn('Self.Destroy;');
    Fps.DecI;
    Fps.WriteLn('end;');

    Fps.WriteLn;
    Fps.WriteLn('procedure _RefMethodPtr(env: PJNIEnv; const m: TMethod; AddRef: boolean);');
    Fps.WriteLn('var i: integer;');
    Fps.WriteLn('begin');
    Fps.IncI;
    Fps.WriteLn('i:=-integer(ptruint(m.Data));');
    Fps.WriteLn(Format('if (i < 1) or (i > %d) then exit;', [MaxMethodPointers]));
    Fps.WriteLn('_MethodPointersCS.Enter;');
    Fps.WriteLn('try');
    Fps.IncI;
    Fps.WriteLn('with _MethodPointers[i - 1] do');
    Fps.WriteLn('if AddRef then InterlockedIncrement(RefCnt) else Release(env);', 1);
    Fps.WriteLn('finally', -1);
    Fps.WriteLn('_MethodPointersCS.Leave;');
    Fps.DecI;
    Fps.WriteLn('end;');
    Fps.DecI;
    Fps.WriteLn('end;');

    Fps.WriteLn;
    Fps.WriteLn('function _CreateMethodPtrObject(env: PJNIEnv; const m: TMethod; const ci: _TJavaClassInfo): jobject;');
    Fps.WriteLn('var i: integer;');
    Fps.WriteLn('var mpi: _TMethodPtrInfo;');
    Fps.WriteLn('begin');
    Fps.IncI;
    Fps.WriteLn('_MethodPointersCS.Enter;');
    Fps.WriteLn('try');
    Fps.IncI;
    Fps.WriteLn('i:=-integer(ptruint(m.Data));');
    Fps.WriteLn(Format('if (i > 0) and (i <= %d) then begin', [MaxMethodPointers]));
    Fps.WriteLn('mpi:=_MethodPointers[i - 1];', 1);
    Fps.WriteLn('InterlockedIncrement(mpi.RefCnt);', 1);
    Fps.WriteLn('end');
    Fps.WriteLn('else begin');
    Fps.WriteLn('mpi:=_TMethodPtrInfo.Create(env, nil, '''', '''');', 1);
    Fps.WriteLn('mpi.RealMethod:=m;', 1);
    Fps.WriteLn('end;');
    Fps.WriteLn('finally', -1);
    Fps.WriteLn('_MethodPointersCS.Leave;');
    Fps.DecI;
    Fps.WriteLn('end;');
    Fps.WriteLn('Result:=_CreateJavaObj(env, pointer(mpi), ci);');
    Fps.DecI;
    Fps.WriteLn('end;');

    // Set support
    Fps.WriteLn;
    Fps.WriteLn('function _GetIntObjValue(env: PJNIEnv; jobj: jobject; const ci: _TJavaClassInfo): longint;');
    Fps.WriteLn('begin');
    Fps.IncI;
    Fps.WriteLn('if jobj = nil then raise Exception.Create(''Attempt to access a NULL set.'');');
    Fps.WriteLn('Result:=env^^.GetIntField(env, jobj, ci.ObjFieldId);');
    Fps.DecI;
    Fps.WriteLn('end;');
    Fps.WriteLn;
    Fps.WriteLn('function _CreateIntObj(env: PJNIEnv; Value: longint; const ci: _TJavaClassInfo): jobject;');
    Fps.WriteLn('begin');
    Fps.IncI;
    Fps.WriteLn('Result:=nil;');
    Fps.WriteLn('Result:=env^^.AllocObject(env, ci.ClassRef);');
    Fps.WriteLn('if Result = nil then exit;');
    Fps.WriteLn('env^^.SetIntField(env, Result, ci.ObjFieldId, Value);');
    Fps.DecI;
    Fps.WriteLn('end;');

    // Write units
    for i:=0 to p.Units.Count - 1 do
      with TUnitDef(p.Units[i]) do begin
        WriteUnit(TUnitDef(p.Units[i]));
      end;

    WriteOnLoad;

    Fps.WriteLn;
    Fps.WriteLn('begin');
    Fps.WriteLn('IsMultiThread:=True;', 1);
    Fps.WriteLn('_MethodPointersCS:=TCriticalSection.Create;', 1);
    Fps.WriteLn('end.');
  finally
    Fps.Free;
    p.Free;
  end;
end;

end.

