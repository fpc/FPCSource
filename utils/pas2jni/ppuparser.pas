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
    procedure ReadUnit(const AName: string; Lines: TStrings);
    function InternalParse(const AUnitName: string): TUnitDef;
  public
    SearchPath: TStringList;
    Units: TDef;

    constructor Create(const ASearchPath: string);
    destructor Destroy; override;
    procedure Parse(const AUnitName: string);
    property OnCheckItem: TOnCheckItem read FOnCheckItem write FOnCheckItem;
  end;

var
  ppudumpprog: string;

implementation

uses process, pipes;

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

procedure TPPUParser.ReadUnit(const AName: string; Lines: TStrings);
var
  p: TProcess;
  s, un: ansistring;
  i, j: integer;
begin
  un:=FindUnit(AName);
  p:=TProcess.Create(nil);
  try
    if ppudumpprog = '' then begin
      // Check for ppudump in the same folder as pas2jni
      ppudumpprog:=ExtractFilePath(ParamStr(0)) + 'ppudump' + ExtractFileExt(ParamStr(0));
      if not FileExists(ppudumpprog) then
        ppudumpprog:='ppudump';
    end;
    p.Executable:=ppudumpprog;
    p.Parameters.Add(un);
    p.Options:=[poUsePipes, poNoConsole, poStderrToOutPut];
    p.ShowWindow:=swoHIDE;
    p.StartupOptions:=[suoUseShowWindow];
    try
      p.Execute;
    except
      raise Exception.CreateFmt('Unable to run "%s".'+LineEnding+'%s', [p.Executable, Exception(ExceptObject).Message]);
    end;
    s:='';
    repeat
      with p.Output do
        while NumBytesAvailable > 0 do begin
          i:=NumBytesAvailable;
          j:=Length(s);
          SetLength(s, j + i);
          ReadBuffer(s[j + 1], i);
        end;
    until not p.Running;
    if p.ExitStatus <> 0 then begin
      if Length(s) > 300 then
        s:='';
      raise Exception.CreateFmt('Error reading contents of unit "%s" using "%s".'+LineEnding+'Error code: %d'+LineEnding+'%s', [un, ppudumpprog, p.ExitStatus, s]);
    end;
  finally
    p.Free;
  end;
  Lines.Text:=s;
{$ifopt D+}
//  Lines.SaveToFile(AName + '-dump.txt');
{$endif}
end;

const
  LInc = 4;
  SDefId = '** Definition Id ';
  SSymId = '** Symbol Id ';

function TPPUParser.InternalParse(const AUnitName: string): TUnitDef;
var
  FLines: TStringList;
  deref: array of TUnitDef;
  CurUnit: TUnitDef;
  CurDef: TDef;
  level, skiplevel: integer;
  IsSystemUnit: boolean;
  AMainUnit: boolean;

  function _ThisLevel(const s: string): boolean;
  var
    i: integer;
  begin
    Result:=True;
    if (level = 1) or (Length(s) < level - LInc) then
      exit;
    if s[1] = '-' then begin
      Result:=False;
      exit;
    end;
    i:=level;
    repeat
      Dec(i, LInc);
      if Copy(s, i, 3) = '** ' then begin
        Result:=False;
        exit;
      end;
    until i = 1;
  end;

  function _GetDef(const Path: string; ExpectedClass: TDefClass = nil): TDef;
  var
    s, ss: string;
    i, j: integer;
    u: TUnitDef;
  begin
    Result:=nil;
    u:=CurUnit;
    s:=Trim(Path);
    if Copy(s, 1, 1) = '(' then begin
      i:=Pos(') ', s);
      if i > 0 then
        Delete(s, 1, i + 1);
    end;
    i:=1;
    while True do begin
      ss:=Trim(ExtractWord(i, s, [',']));
      if ss = '' then
        break;
      if Pos('Unit', ss) = 1 then begin
        j:=StrToInt(Copy(ss, 6, MaxInt));
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
      end
      else
      if Pos('DefId', ss) = 1 then begin
        j:=StrToInt(Copy(ss, 7, MaxInt));
        Result:=u.FindDef(j);
        if Result = nil then begin
          if ExpectedClass <> nil then
            Result:=ExpectedClass.Create(u, dtNone)
          else
            Result:=TDef.Create(u, dtNone);
          Result.DefId:=j;
        end;
        break;
      end;
      Inc(i);
    end;
    if (ExpectedClass <> nil) and (Result <> nil) then
      if (Result.DefType <> dtNone) and not (Result is ExpectedClass) then
        raise Exception.CreateFmt('Unexpected class. Expected: %s, got: %s', [ExpectedClass.ClassName, Result.ClassName]);
  end;

  function _ReadSym(var idx: integer; ParentDef: TDef): TDef;
  var
    s, ss, name: string;
    id: integer;
    i, j: integer;
    d: TDef;
  begin
    Result:=nil;
    // symvol id
    s:=Trim(FLines[idx]);
    id:=StrToInt(ExtractWord(4, s, [' ']));
    Inc(idx);
    s:=Trim(FLines[idx]);
    if Pos('Property', s) = 1 then begin
      name:=Trim(Copy(s, 10, MaxInt));
      Result:=TVarDef.Create(nil, dtProp);
      TVarDef(Result).VarOpt:=[];
    end
    else begin
      i:=Pos('symbol', s);
      if i = 0 then
        exit;
      name:=Trim(Copy(s, i + 7, MaxInt));
      if Copy(name, 1, 1) = '$' then
        exit;

      s:=LowerCase(Trim(Copy(s, 1, i - 1)));
      if s = 'field variable' then
        Result:=TVarDef.Create(nil, dtField)
      else
      if s = 'global variable' then
        Result:=TVarDef.Create(nil, dtVar)
      else
      if s = 'parameter variable' then begin
        Result:=TVarDef.Create(nil, dtParam);
        TVarDef(Result).VarOpt:=[voRead];
      end
      else
      if s = 'enumeration' then begin
        if ParentDef = CurUnit then
          exit;
        Result:=TConstDef.Create(nil, dtConst);
        TConstDef(Result).VarType:=ParentDef;
      end
      else
      if s = 'constant' then begin
        Result:=TConstDef.Create(nil, dtConst);
      end

      else
      if (s = 'procedure') or (s = 'type') then
        Result:=nil
      else
        exit;
    end;

    if Result <> nil then begin
      Result.Name:=name;
      Result.SymId:=id;
    end;

    Inc(level, LInc);
    skiplevel:=level;
    Inc(idx);
    while idx < FLines.Count do begin
      s:=FLines[idx];
      if not _ThisLevel(s) or (Copy(Trim(s), 1, 3) = '---') then begin
        Dec(idx);
        break;
      end;

      if Pos('Visibility :', s) > 0 then begin
        s:=LowerCase(Trim(ExtractWord(2, s, [':'])));
        if (s <> 'public') and (s <> 'published') then begin
          FreeAndNil(Result);
          exit;
        end;
      end
      else
      if (Pos('Definition :', s) > 0) or (Pos('Result Type :', s) > 0) then begin
        if (Result = nil) or (Result.DefType <> dtConst) then begin
          s:=Trim(ExtractWord(2, s, [':']));
          d:=_GetDef(s);
          if (d <> nil) and (d.Name = '') then begin
            if (d.DefType = dtProc) and (TProcDef(d).ProcType = ptConstructor) and (CompareText(name, 'create') = 0) then
              name:='Create'; // fix char case for standard constructors
            d.Name:=name;
            d.SymId:=id;
          end;
        end
      end
      else
      if Pos('Options :', s) > 0 then begin
        s:=LowerCase(Trim(ExtractWord(2, s, [':'])));
        if Pos('hidden', s) > 0 then begin
          FreeAndNil(Result);
          exit;
        end;
      end
      else
      if Result <> nil then
        case Result.DefType of
          dtVar, dtField, dtProp, dtParam:
            if (Pos('Var Type :', s) > 0) or (Pos('Prop Type :', s) > 0) then begin
              s:=Trim(ExtractWord(2, s, [':']));
              TVarDef(Result).VarType:=_GetDef(s);
            end
            else
            if Pos('access :', s) > 0 then begin
              if Pos('Sym:', Trim(FLines[idx+1])) = 1 then begin
                d:=nil;
                ss:=Trim(ExtractWord(2, s, [':']));
                if Pos('Nil', ss) = 0 then
                  d:=_GetDef(ss, TProcDef);
                with TVarDef(Result) do
                  if Pos('Readaccess :', s) > 0 then begin
                    VarOpt:=VarOpt + [voRead];
                    if (d <> nil) and (d.Count = 1) then
                      IndexType:=TVarDef(d[0]).VarType;
                  end
                  else
                    if Pos('Writeaccess :', s) > 0 then begin
                      VarOpt:=VarOpt + [voWrite];
                      if (d <> nil) and (d.Count = 2) then
                        IndexType:=TVarDef(d[0]).VarType;
                    end;
              end;
            end
            else
            if Pos('Spez :', s) > 0 then begin
              with TVarDef(Result) do begin
                s:=LowerCase(Trim(ExtractWord(2, s, [':'])));
                if s = 'out' then
                  VarOpt:=[voWrite, voOut]
                else
                if s = 'var' then
                  VarOpt:=[voRead, voWrite, voVar]
                else
                if s = 'const' then
                  VarOpt:=[voRead, voConst];
              end;
            end;

          dtConst:
            begin
              j:=Pos('Value :', s);
              if j > 0 then begin
                Inc(j, 6);
                ss:=Trim(Copy(s, j + 1, MaxInt));
                if Copy(ss, 1, 1) = '"' then begin
                  Delete(ss, 1, 1);
                  i:=level - LInc;
                  while True do begin
                    Inc(idx);
                    if idx >= FLines.Count then
                      break;
                    s:=FLines[idx];
                    if (Copy(s, i, 3) = '** ') or (Copy(s, j, 1) = ':') then
                      break;
                    ss:=ss + #10 + s;
                  end;
                  Dec(idx);
                  Delete(ss, Length(ss), 1);
                  ss:=StringReplace(ss, '\', '\\', [rfReplaceAll]);
                  ss:=StringReplace(ss, '"', '\"', [rfReplaceAll]);
                  ss:=StringReplace(ss, #10, '\n', [rfReplaceAll]);
                  ss:='"' + ss + '"';
                end;
                TConstDef(Result).Value:=ss;
              end
              else
              if Pos('OrdinalType :', s) > 0 then begin
                s:=Trim(ExtractWord(2, s, [':']));
                TConstDef(Result).VarType:=_GetDef(s);
              end
              else
              if Pos('Set Type :', s) > 0 then begin
//                s:=Trim(ExtractWord(2, s, [':']));
//                TConstDef(Result).VarType:=_GetDef(s);
                FreeAndNil(Result);
                exit;
              end;
            end;
        end;

      Inc(idx);
    end;

    if Result <> nil then
      ParentDef.Add(Result);
  end;

  procedure _RemoveCurDef;
  var
    d: TDef;
  begin
    d:=CurDef;
    CurDef:=CurDef.Parent;
    d.Free;
    skiplevel:=level;
  end;

var
  s: ansistring;
  i, j: integer;
  dd: TDef;
  HdrRead: boolean;
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

  FLines:=TStringList.Create;
  try
    ReadUnit(AUnitName, FLines);

    IsSystemUnit:=CompareText(AUnitName, 'system') = 0;

    Result:=TUnitDef.Create(nil, dtUnit);
    Units.Add(Result);
    CurUnit:=Result;
    SetLength(deref, 0);
    CurDef:=Result;
    level:=1;
    skiplevel:=0;
    i:=-1;
    HdrRead:=False;
    while True do begin
      Inc(i);
      if i >= FLines.Count then
        break;
      s:=FLines[i];

      if s = 'Implementation symtable' then
        break;

      if not HdrRead then begin
        if Trim(s) = 'Interface symtable' then begin
          HdrRead:=True;
          continue;
        end;

        if Pos('Analyzing', s) = 1 then begin
          j:=Pos('(v', s);
          if j > 0 then
            Result.PPUVer:=StrToInt(Copy(s, j + 2, Length(s) - j - 2));
        end
        else
        if Pos('Target processor', s) = 1 then
          Result.CPU:=Trim(ExtractWord(2, s, [':']))
        else
        if Pos('Target operating system', s) = 1 then
          Result.OS:=Trim(ExtractWord(2, s, [':']))
        else
        if Pos('Interface Checksum', s) = 1 then
          Result.IntfCRC:=Trim(ExtractWord(2, s, [':']))
        else
        if (Pos('Module Name:', s) = 1) and (Result.Name = '') then begin
          Result.Name:=Trim(ExtractWord(2, s, [':']));
          continue;
        end
        else
        if Pos('DerefMap[', s) = 1 then begin
          s:=Trim(ExtractWord(2, s, ['=']));
          j:=Length(deref);
          SetLength(deref, j + 1);
          deref[j]:=TUnitDef.Create(nil, dtNone);
          deref[j].Name:=s;
          continue;
        end;
      end;

      while not _ThisLevel(s) do begin
        if skiplevel = 0 then
          CurDef:=CurDef.Parent;
        Dec(level, LInc);
        skiplevel:=0;
      end;

      if level = skiplevel then
        continue; // Skipping not supported entries

      // Definition
      j:=Pos(SDefId, s);
      if j > 0 then begin
        Inc(level, LInc);
        // def id
        j:=StrToInt(Copy(s, j + Length(SDefId), Length(s) - (j + Length(SDefId)) - 2));
        Inc(i);
        s:=FLines[i];
        if Pos('definition', s) = 0 then begin
          skiplevel:=level;
          continue;
        end;
        s:=LowerCase(Trim(ExtractWord(1, s, [' '])));
        dd:=nil;
        if s = 'object/class' then
          dd:=TClassDef.Create(CurDef, dtClass)
        else
        if s = 'record' then
          dd:=TRecordDef.Create(CurDef, dtRecord)
        else
        if s = 'procedure' then
          dd:=TProcDef.Create(CurDef, dtProc)
        else
        if s = 'ordinal' then begin
          dd:=TTypeDef.Create(CurDef, dtType);
          TTypeDef(dd).BasicType:=btLongInt;
        end
        else
        if Pos('string', s) > 0 then begin
          dd:=TTypeDef.Create(CurDef, dtType);
          dd.Name:=s;
          if (s = 'widestring') or (s = 'unicodestring') then
            TTypeDef(dd).BasicType:=btWideString
          else
            TTypeDef(dd).BasicType:=btString;
        end
        else
        if s = 'float' then begin
          dd:=TTypeDef.Create(CurDef, dtType);
          TTypeDef(dd).BasicType:=btDouble;
        end
        else
        if s = 'enumeration' then begin
          dd:=TTypeDef.Create(CurDef, dtEnum);
          TTypeDef(dd).BasicType:=btEnum;
        end
        else
        if s = 'pointer' then begin
          dd:=TTypeDef.Create(CurDef, dtType);
          TTypeDef(dd).BasicType:=btPointer;
        end
        else
        if s = 'procedural' then begin
          dd:=TProcDef.Create(CurDef, dtProcType);
          TProcDef(dd).ProcType:=ptProcedure;
        end
        else
        if s = 'set' then begin
          dd:=TSetDef.Create(CurDef, dtSet);
        end
        else
          skiplevel:=level;
        if dd <> nil then begin
          CurDef:=dd;
          CurDef.DefId:=j;
        end;
        continue;
      end;

      // Symbol
      if Pos(SSymId, s) > 0 then begin
        dd:=_ReadSym(i, CurDef);
        continue;
      end;

      if CurDef <> nil then
        case CurDef.DefType of
          dtClass:
            begin
              if Pos('Type :', Trim(s)) = 1 then begin
                s:=LowerCase(Trim(ExtractWord(2, s, [':'])));
                if CurDef.DefId = 0 then
                  s:=s;
                if s <> 'class' then
                  _RemoveCurDef;
              end
              else
              if Pos('Ancestor Class :', s) > 0 then begin
                s:=Trim(ExtractWord(2, s, [':']));
                TClassDef(CurDef).AncestorClass:=TClassDef(_GetDef(s, TClassDef));
              end
            end;
          dtRecord:
            begin
              if IsSystemUnit and (Pos('Name of Record :', s) > 0) then begin
                s:=Trim(ExtractWord(2, s, [':']));
                if CompareText(s, 'tguid') = 0 then begin
                  dd:=TTypeDef.Create(CurUnit, dtType);
                  TTypeDef(dd).BasicType:=btGuid;
                  dd.DefId:=CurDef.DefId;
                  CurDef.Free;
                  CurDef:=dd;
                end;
              end
              else
              if Pos('DataSize :', s) > 0 then begin
                s:=Trim(ExtractWord(2, s, [':']));
                TRecordDef(CurDef).Size:=StrToInt(s);
              end;
            end;
          dtProc, dtProcType:
            begin
              s:=Trim(s);
              if Pos('TypeOption :', s) = 1 then begin
                s:=LowerCase(Trim(ExtractWord(2, s, [':'])));
                with TProcDef(CurDef) do
                  if s = 'procedure' then
                    ProcType:=ptProcedure
                  else
                  if s = 'function' then
                    ProcType:=ptFunction
                  else
                  if s = 'constructor' then
                    ProcType:=ptConstructor
                  else
                  if s = 'destructor' then
                    ProcType:=ptDestructor;
              end
              else
              if Pos('Return type :', s) = 1 then begin
                s:=Trim(ExtractWord(2, s, [':']));
                with TProcDef(CurDef) do begin
                  ReturnType:=_GetDef(s);
                  if (CurDef.DefType = dtProcType) and not ( (ReturnType is TTypeDef) and (TTypeDef(ReturnType).BasicType = btVoid) ) then
                    ProcType:=ptFunction;
                end;
              end
              else
              if Pos('Visibility :', s) = 1 then begin
                s:=LowerCase(Trim(ExtractWord(2, s, [':'])));
                if (s <> 'public') and (s <> 'published') then
                  CurDef.IsPrivate:=True;
              end
              else
              if Pos('Options :', s) = 1 then begin
                s:=LowerCase(Trim(ExtractWord(2, s, [':'])));
                with TProcDef(CurDef) do begin
                  if Pos('overridingmethod', s) > 0 then begin
                    ProcOpt:=ProcOpt + [poOverride];
                    if ProcType <> ptConstructor then
                      CurDef.IsPrivate:=True;
                  end;
                  if Pos('overload', s) > 0 then
                    ProcOpt:=ProcOpt + [poOverload];
                  if Pos('methodpointer', s) > 0 then
                    ProcOpt:=ProcOpt + [poMethodPtr];

                  if (CurDef.Parent.DefType = dtClass) and (Pos('abstractmethod', s) > 0) then
                    TClassDef(CurDef.Parent).HasAbstractMethods:=True;
                end;
              end;
            end;
          dtType:
            with TTypeDef(CurDef) do
              if Pos('Base type :', s) > 0 then begin
                s:=LowerCase(Trim(ExtractWord(2, s, [':'])));
                if Pos('bool', s) = 1 then
                  BasicType:=btBoolean
                else
                if s = 'u8bit' then
                  BasicType:=btByte
                else
                if s = 's8bit' then
                  BasicType:=btShortInt
                else
                if s = 'u16bit' then
                  BasicType:=btWord
                else
                if s = 's16bit' then
                  BasicType:=btSmallInt
                else
                if s = 'u32bit' then
                  BasicType:=btLongWord
                else
                if s = 's32bit' then
                  BasicType:=btLongInt
                else
                if (s = 'u64bit') or (s = 's64bit') then
                  BasicType:=btInt64
                else
                if s = 'uvoid' then
                  BasicType:=btVoid
                else
                if s = 'uchar' then
                  BasicType:=btChar
                else
                if s = 'uwidechar' then
                  BasicType:=btWideChar;
              end
              else
              if Pos('Float type :', s) > 0 then begin
                s:=Trim(ExtractWord(2, s, [':']));
                if s = '0' then
                  BasicType:=btSingle;
              end
              else
              if Pos('Range :', s) > 0 then begin
                s:=LowerCase(Trim(ExtractWord(2, s, [':'])));
                if s = '0 to 1' then
                  BasicType:=btBoolean;
              end;
          dtSet:
            with TSetDef(CurDef) do
              if Pos('Size :', s) > 0 then
                Size:=StrToInt(Trim(ExtractWord(2, s, [':'])))
              else
              if Pos('Set Base :', s) > 0 then
                Base:=StrToInt(Trim(ExtractWord(2, s, [':'])))
              else
              if Pos('Set Max :', s) > 0 then
                ElMax:=StrToInt(Trim(ExtractWord(2, s, [':'])))
              else
              if Pos('Element type :', s) > 0 then
                ElType:=TTypeDef(_GetDef(Trim(ExtractWord(2, s, [':'])), TTypeDef))
              else
              if Pos('Type symbol :', s) > 0 then begin
                s:=LowerCase(Trim(ExtractWord(2, s, [':'])));
                if Trim(ExtractWord(2, s, [' '])) = 'nil' then
                  _RemoveCurDef;
              end;
        end;
    end;

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
    FLines.Free;
  end;
end;

end.

