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

unit ppuparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, def;

type
  TCheckItemResult = (crDefault, crInclude, crExclude);
  TOnCheckItem = function (const ItemName: string): TCheckItemResult of object;

  { TPPUParser }
  TPPUParser = class
  private
    FOnCheckItem: TOnCheckItem;
    function FindUnit(const AName: string): string;
    function ReadUnit(const AName: string): string;
    function InternalParse(const AUnitName: string): TUnitDef;
  public
    SearchPath: TStringList;
    Units: TDef;
    OnExceptionProc: TProcDef;

    constructor Create(const ASearchPath: string);
    destructor Destroy; override;
    procedure Parse(const AUnitName: string);
    property OnCheckItem: TOnCheckItem read FOnCheckItem write FOnCheckItem;
  end;

var
  ppudumpprog: string;

implementation

uses process, pipes, fpjson, jsonparser;

const
  OnExceptionProcName = 'JNI_OnException';

type
  TCharSet = set of char;

function WordPosition(const N: Integer; const S: string;
  const WordDelims: TCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do
  begin
    { skip over delimiters }
    while (I <= Length(S)) and (S[I] in WordDelims) do
      Inc(I);
    { if we're not beyond end of S, we're at the start of a word }
    if I <= Length(S) then
      Inc(Count);
    { if not finished, find the end of the current word }
    if Count <> N then
      while (I <= Length(S)) and not (S[I] in WordDelims) do
        Inc(I)
    else
      Result := I;
  end;
end;

function ExtractWord(N: Integer; const S: string;
  const WordDelims: TCharSet): string;
var
  I: Integer;
  Len: Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if I <> 0 then
    { find the end of the current word }
    while (I <= Length(S)) and not (S[I] in WordDelims) do
    begin
      { add the I'th character to result }
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;

{ TPPUParser }

constructor TPPUParser.Create(const ASearchPath: string);
var
  i, j: integer;
  s, d: string;
  sr: TSearchRec;
begin
  SearchPath:=TStringList.Create;
  SearchPath.Delimiter:=';';
  SearchPath.DelimitedText:=ASearchPath;
  i:=0;
  while i < SearchPath.Count do begin
    s:=SearchPath[i];
    if (Pos('*', s) > 0) or (Pos('?', s) > 0) then begin
      d:=ExtractFilePath(s);
      j:=FindFirst(s, faDirectory, sr);
      while j = 0 do begin
        if (sr.Name <> '.') and (sr.Name <> '..') then
          SearchPath.Add(d + sr.Name);
        j:=FindNext(sr);
      end;
      FindClose(sr);
      SearchPath.Delete(i);
    end
    else
      Inc(i);
  end;
  Units:=TDef.Create(nil, dtNone);
end;

destructor TPPUParser.Destroy;
begin
  Units.Free;
  SearchPath.Free;
  inherited Destroy;
end;

procedure TPPUParser.Parse(const AUnitName: string);
begin
  InternalParse(AUnitName);
end;

function TPPUParser.FindUnit(const AName: string): string;
var
  i: integer;
  fn: string;
begin
  fn:=ChangeFileExt(LowerCase(AName), '.ppu');
  if FileExists(fn) then begin
    Result:=fn;
    exit;
  end;
  for i:=0 to SearchPath.Count - 1 do begin
    Result:=IncludeTrailingPathDelimiter(SearchPath[i]) + fn;
    if FileExists(Result) then
      exit;
  end;
  raise Exception.CreateFmt('Unable to find PPU file for unit "%s".', [AName]);
end;

function TPPUParser.ReadUnit(const AName: string): string;

  procedure _ReadOutput(o: TInputPipeStream; var s: string);
  var
    i, j: integer;
  begin
    with o do
      while NumBytesAvailable > 0 do begin
        i:=NumBytesAvailable;
        j:=Length(s);
        SetLength(s, j + i);
        ReadBuffer(s[j + 1], i);
      end;
  end;

var
  p: TProcess;
  s, un, err: ansistring;
  ec: integer;
begin
  un:=FindUnit(AName);
  p:=TProcess.Create(nil);
  try
    if ppudumpprog = '' then begin
      ppudumpprog:='ppudump';
      // Check for ppudump in the same folder as pas2jni
      s:=ExtractFilePath(ParamStr(0));
      if s <> '' then begin
        s:=s + ppudumpprog + ExtractFileExt(ParamStr(0));
        if FileExists(s) then
          ppudumpprog:=s;
      end;
    end;
    p.Executable:=ppudumpprog;
    p.Parameters.Add('-Fj');
    p.Parameters.Add(un);
    p.Options:=[poUsePipes, poNoConsole];
    p.ShowWindow:=swoHIDE;
    p.StartupOptions:=[suoUseShowWindow];
    try
      p.Execute;
    except
      raise Exception.CreateFmt('Unable to run "%s".'+LineEnding+'%s', [p.Executable, Exception(ExceptObject).Message]);
    end;
    s:='';
    err:='';
    repeat
      _ReadOutput(p.Output, s);
      _ReadOutput(p.Stderr, err);
    until not p.Running;
    ec:=p.ExitStatus;
    if Copy(s, 1, 1) <> '[' then begin
      ec:=-1;
      err:='Output of ppudump is not in JSON format.' + LineEnding + 'Probably old version of ppudump has been used.';
    end;
    if ec <> 0 then begin
      if err = '' then
        if Length(s) < 300 then
          err:=s;
      raise Exception.CreateFmt('Error reading contents of unit "%s" using "%s".'+LineEnding+'Error code: %d'+LineEnding+'%s', [un, ppudumpprog, ec, err]);
    end;
  finally
    p.Free;
  end;
  Result:=s;
{$ifopt D+}
//  Lines.SaveToFile(AName + '-dump.txt');
{$endif}
end;

function TPPUParser.InternalParse(const AUnitName: string): TUnitDef;
var
  junit: TJSONObject;
  jp: TJSONParser;
  deref: array of TUnitDef;
  CurUnit: TUnitDef;
  IsSystemUnit: boolean;
  AMainUnit: boolean;
  CurObjName: string;

  function _GetRef(Ref: TJSONObject; ExpectedClass: TDefClass = nil): TDef;
  var
    j: integer;
    u: TUnitDef;
  begin
    Result:=nil;
    if Ref = nil then
      exit;
    u:=CurUnit;
    j:=Ref.Get('Unit', -1);
    if j >= 0 then begin
      u:=deref[j];
      if u.DefType = dtNone then begin
        // Reading unit
        u:=InternalParse(LowerCase(u.Name));
        if u = nil then
          exit;
        if u.CPU <> CurUnit.CPU then
          raise Exception.CreateFmt('Invalid target CPU of unit "%s": %s', [u.Name, u.CPU]);
        if u.OS <> CurUnit.OS then
          raise Exception.CreateFmt('Invalid target OS of unit "%s": %s', [u.Name, u.OS]);
        if u.PPUVer <> CurUnit.PPUVer then
          raise Exception.CreateFmt('Invalid PPU version of unit "%s": %s', [u.Name, u.PPUVer]);
        deref[j].Free;
        deref[j]:=u;
      end;
    end;

    j:=Ref.Integers['Id'];
    Result:=u.FindDef(j);
    if Result = nil then begin
      if ExpectedClass <> nil then
        Result:=ExpectedClass.Create(u, dtNone)
      else
        Result:=TDef.Create(u, dtNone);
      Result.DefId:=j;
    end;

    if (ExpectedClass <> nil) and (Result <> nil) then
      if (Result.DefType <> dtNone) and not (Result is ExpectedClass) then
        raise Exception.CreateFmt('Unexpected class. Expected: %s, got: %s', [ExpectedClass.ClassName, Result.ClassName]);
  end;

  procedure _ReadDefs(CurDef: TDef; jobj: TJSONObject; const ItemsName: string);
  var
    i, j: integer;
    jt, s: string;
    d: TDef;
    it: TJSONObject;
    jarr, arr: TJSONArray;
  begin
    jarr:=jobj.Get(ItemsName, TJSONArray(nil));
    if jarr = nil then
      exit;
    with jarr do
      for i:=0 to Count - 1 do begin
        it:=Objects[i];
        CurObjName:=it.Get('Name', '');
        jt:=it.Strings['Type'];
        if jt = 'obj' then begin
          if it.Strings['ObjType'] <> 'class' then
            continue;
          d:=TClassDef.Create(CurDef, dtClass);
        end
        else
        if jt = 'rec' then begin
          if IsSystemUnit and (CompareText(CurObjName, 'tguid') = 0) then begin
            d:=TTypeDef.Create(CurDef, dtType);
            TTypeDef(d).BasicType:=btGuid;
          end
          else
            d:=TRecordDef.Create(CurDef, dtRecord);
        end
        else
        if jt = 'proc' then
          d:=TProcDef.Create(CurDef, dtProc)
        else
        if jt = 'proctype' then begin
          d:=TProcDef.Create(CurDef, dtProcType);
          TProcDef(d).ProcType:=ptProcedure;
        end
        else
        if jt = 'param' then begin
          d:=TVarDef.Create(CurDef, dtParam);
          TVarDef(d).VarOpt:=[voRead];
        end
        else
        if jt = 'prop' then begin
          d:=TVarDef.Create(CurDef, dtProp);
          TVarDef(d).VarOpt:=[];
        end
        else
        if jt = 'field' then
          d:=TVarDef.Create(CurDef, dtField)
        else
        if jt = 'var' then
          d:=TVarDef.Create(CurDef, dtVar)
        else
        if jt = 'ord' then begin
          d:=TTypeDef.Create(CurDef, dtType);
          with TTypeDef(d) do begin
            s:=it.Strings['OrdType'];
            j:=it.Get('Size', 0);
            if s = 'void' then
              BasicType:=btVoid
            else
            if s = 'uint' then begin
              case j of
                1: BasicType:=btByte;
                2: BasicType:=btWord;
                4: BasicType:=btLongWord;
                else BasicType:=btInt64;
              end;
            end
            else
            if s = 'sint' then begin
              case j of
                1: BasicType:=btShortInt;
                2: BasicType:=btSmallInt;
                4: BasicType:=btLongInt;
                else BasicType:=btInt64;
              end;
            end
            else
            if (s = 'pasbool') or (s = 'bool') then
              BasicType:=btBoolean
            else
            if s = 'char' then begin
              if j = 1 then
                BasicType:=btChar
              else
                BasicType:=btWideChar;
            end
            else
            if s = 'currency' then
              BasicType:=btDouble;
          end;
        end
        else
        if jt = 'float' then begin
          d:=TTypeDef.Create(CurDef, dtType);
          with TTypeDef(d) do
            if it.Strings['FloatType'] = 'single' then
              BasicType:=btSingle
            else
              BasicType:=btDouble;
        end
        else
        if jt = 'string' then begin
          d:=TTypeDef.Create(CurDef, dtType);
          s:=it.Strings['StrType'];
          with TTypeDef(d) do
            if (s = 'wide') or (s = 'unicode') or (s = 'long') then
              BasicType:=btWideString
            else
              BasicType:=btString;
          if not (IsSystemUnit and (CompareText(CurObjName, 'rawbytestring') = 0)) then
            CurObjName:=s + 'string';
        end
        else
        if jt = 'enum' then begin
          d:=TTypeDef.Create(CurDef, dtEnum);
          TTypeDef(d).BasicType:=btEnum;
        end
        else
        if jt = 'set' then
          d:=TSetDef.Create(CurDef, dtSet)
        else
        if jt = 'ptr' then begin
          d:=TTypeDef.Create(CurDef, dtType);
          TTypeDef(d).BasicType:=btPointer;
        end
        else
        if jt = 'const' then
          d:=TConstDef.Create(CurDef, dtConst)
        else
          continue;

        if (CurObjName = '') and (d.DefType <> dtEnum) then begin
          d.Free;
          continue;
        end;

        // Common def attributes
        d.Name:=CurObjName;
        d.DefId:=it.Get('Id', -1);
        d.SymId:=it.Get('SymId', -1);
        s:=it.Get('Visibility', '');
        d.IsPrivate:=(s <> '') and (s <> 'public') and (s <> 'published');
        if Copy(d.Name, 1, 1) = '$' then
          d.IsPrivate:=True;

        // Specific def attributes
        case d.DefType of
          dtClass:
            with TClassDef(d) do begin
              AncestorClass:=TClassDef(_GetRef(it.Get('Ancestor', TJSONObject(nil)), TClassDef));
              _ReadDefs(d, it, 'Fields');
            end;
          dtRecord:
            with TRecordDef(d) do begin
              Size:=it.Integers['Size'];
              _ReadDefs(d, it, 'Fields');
            end;
          dtProc, dtProcType:
            with TProcDef(d) do begin
              arr:=it.Get('Options', TJSONArray(nil));
              if arr <> nil then
                for j:=0 to arr.Count - 1 do begin
                  s:=arr.Strings[j];
                  if s = 'procedure' then
                    ProcType:=ptProcedure
                  else
                  if s = 'function' then
                    ProcType:=ptFunction
                  else
                  if s = 'constructor' then begin
                    ProcType:=ptConstructor;
                    if CompareText(Name, 'create') = 0 then
                      Name:='Create'; // fix char case for standard constructors
                  end
                  else
                  if s = 'destructor' then
                    ProcType:=ptDestructor
                  else
                  if s = 'overriding' then begin
                    ProcType:=ptDestructor;
                    ProcOpt:=ProcOpt + [poOverride];
                    if ProcType <> ptConstructor then
                      IsPrivate:=True;
                  end
                  else
                  if s = 'overload' then
                    ProcOpt:=ProcOpt + [poOverload]
                  else
                  if s = 'abstract' then
                    TClassDef(Parent).HasAbstractMethods:=True;
                end;

                ReturnType:=_GetRef(it.Get('RetType', TJSONObject(nil)));
                if (DefType = dtProcType) and not ( (ReturnType is TTypeDef) and (TTypeDef(ReturnType).BasicType = btVoid) ) then
                  ProcType:=ptFunction;
                if it.Get('MethodPtr', False) then
                  ProcOpt:=ProcOpt + [poMethodPtr];

                if IsSystemUnit and (ProcType = ptFunction) and (Name = 'int') then
                  Name:='Int';

              _ReadDefs(d, it, 'Params');
              // Check for user exception handler proc
              if AMainUnit and (Parent = CurUnit) and (OnExceptionProc = nil) and (AnsiCompareText(Name, OnExceptionProcName) = 0) then
                OnExceptionProc:=TProcDef(d);
            end;
          dtVar, dtField, dtParam:
            with TVarDef(d) do begin
              VarType:=_GetRef(it.Objects['VarType']);
              s:=it.Get('Spez', '');
              if s = 'out' then
                VarOpt:=[voWrite, voOut]
              else
              if s = 'var' then
                VarOpt:=[voRead, voWrite, voVar]
              else
              if s = 'const' then
                VarOpt:=[voRead, voConst];
            end;
          dtProp:
            with TVarDef(d) do begin
              VarType:=_GetRef(it.Objects['PropType']);
              if it.Get('Getter', TJSONObject(nil)) <> nil then
                VarOpt:=VarOpt + [voRead];
              if it.Get('Setter', TJSONObject(nil)) <> nil then
                VarOpt:=VarOpt + [voWrite];

              _ReadDefs(d, it, 'Params');
            end;
          dtEnum:
            _ReadDefs(d, it, 'Elements');
          dtSet:
            with TSetDef(d) do begin
              Size:=it.Integers['Size'];
              Base:=it.Integers['Base'];
              ElMax:=it.Integers['Max'];
              ElType:=TTypeDef(_GetRef(it.Objects['ElType'], TTypeDef));
              if (ElType <> nil) and (ElType.Name = '') then
                ElType.Name:=CurObjName + 'El';
            end;
          dtConst:
            with TConstDef(d) do begin
              VarType:=_GetRef(it.Get('TypeRef', TJSONObject(nil)));
              s:=it.Strings['ValType'];
              if s = 'int' then
                Value:=IntToStr(it.Int64s['Value'])
              else
              if s = 'float' then begin
                Str(it.Floats['Value'], s);
                Value:=s;
              end
              else
              if s = 'string' then begin
                s:=it.Strings['Value'];
                s:=StringReplace(s, '\', '\\', [rfReplaceAll]);
                s:=StringReplace(s, '"', '\"', [rfReplaceAll]);
                s:=StringReplace(s, #9, '\t', [rfReplaceAll]);
                s:=StringReplace(s, #10, '\n', [rfReplaceAll]);
                s:=StringReplace(s, #13, '\r', [rfReplaceAll]);
                Value:='"' + s + '"';
              end
              else
                FreeAndNil(d);
            end;
        end;
      end;
  end;

var
  i, j: integer;
  s: string;
begin
  Result:=nil;
  for i:=0 to Units.Count - 1 do
    if CompareText(Units[i].Name, AUnitName) = 0 then begin
      Result:=TUnitDef(Units[i]);
      exit;
    end;

  AMainUnit:=FOnCheckItem(AUnitName) = crInclude;

  if not AMainUnit and ( (CompareText(AUnitName, 'windows') = 0) or (CompareText(AUnitName, 'unix') = 0) ) then begin
    Result:=nil;
    exit;
  end;

  s:=ReadUnit(AUnitName);
  try
    junit:=nil;
    try
      jp:=TJSONParser.Create(s);
      try
        junit:=TJSONObject(jp.Parse.Items[0]);
      finally
        jp.Free;
      end;

      IsSystemUnit:=CompareText(AUnitName, 'system') = 0;

      Result:=TUnitDef.Create(nil, dtUnit);
      Units.Add(Result);
      Result.Name:=junit.Strings['Name'];
      Result.PPUVer:=junit.Integers['Version'];
      Result.CPU:=junit.Strings['TargetCPU'];
      Result.OS:=junit.Strings['TargetOS'];
      Result.IntfCRC:=junit.Strings['InterfaceCRC'];

      if junit.Find('Units') <> nil then
        with junit.Arrays['Units'] do begin
          SetLength(deref, Count);
          for i:=0 to Count - 1 do begin
            deref[i]:=TUnitDef.Create(nil, dtNone);
            deref[i].Name:=Strings[i];
          end;
        end;

      CurUnit:=Result;
      _ReadDefs(CurUnit, junit, 'Interface');

      Result.ResolveDefs;

      if AMainUnit then
        Result.IsUsed:=True;

      SetLength(Result.UsedUnits, Length(deref));
      j:=0;
      for i:=0 to High(deref) do
        if deref[i].DefType = dtNone then
          deref[i].Free
        else begin
          Result.UsedUnits[j]:=deref[i];
          Inc(j);
        end;
      SetLength(Result.UsedUnits, j);
    finally
      junit.Free;
    end;
  except
    if CurObjName <> '' then
      CurObjName:=Format('; Object: "%s"', [CurObjName]);
    raise Exception.CreateFmt('%s' + LineEnding + 'Unit: "%s"%s', [Exception(ExceptObject).Message, AUnitName, CurObjName]);
  end;
end;

end.

