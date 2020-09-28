{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2014 by Michael Van Canneyt

    Unit tests for Pascal-to-Javascript source map.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Examples:
    ./testpas2js --suite=TTestSrcMap.TestEmptyProgram
}
unit TCSrcMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  jstree, jswriter, JSSrcMap,
  FPPas2Js, FPPJsSrcMap,
  tcmodules, PasResolveEval;

type

  { TCustomTestSrcMap }

  TCustomTestSrcMap = class(TCustomTestModule)
  private
    FJS_Writer: TJSWriter;
    FPas2JSMapper: TPas2JSMapper;
    FSrcMap: TPas2JSSrcMap;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function ConvertJSModuleToString(El: TJSElement): string; override;
    procedure CheckSrcMap(const aTitle: string; const JSLines: array of string); virtual;
    procedure WriteSrcMapLine(GeneratedLine: integer);
  public
    property Pas2JSMapper: TPas2JSMapper read FPas2JSMapper; // fills SrcMap
    property SrcMap: TPas2JSSrcMap read FSrcMap; // map container
    property JS_Writer: TJSWriter read FJS_Writer; // JS element to text
  end;

  { TTestSrcMap }

  TTestSrcMap = class(TCustomTestSrcMap)
  published
    procedure TestEmptyProgram;
    procedure TestEmptyUnit;
    procedure TestIf;
    procedure TestIfBegin;
    procedure TestForConstRange;
    procedure TestFunction;
    procedure TestExternalObjCall;
    procedure TestBracketAccessor;
  end;

implementation

{ TCustomTestSrcMap }

procedure TCustomTestSrcMap.SetUp;
begin
  FSrcMap:=TPas2JSSrcMap.Create('test1.js.map');
  FPas2JSMapper:=TPas2JSMapper.Create(4096);
  FPas2JSMapper.SrcMap:=SrcMap;
  SrcMap.Release;// release the refcount from the Create
  //SrcMap.SourceRoot:='';
  //SrcMap.LocalFilename:='';
  fJS_Writer:=TJSWriter.Create(Pas2JSMapper);
  JS_Writer.IndentSize:=2;
  inherited SetUp;
end;

procedure TCustomTestSrcMap.TearDown;
begin
  // Note: SrcMap is freed by freeing Pas2JSMapper
  FreeAndNil(FJS_Writer);
  FreeAndNil(FPas2JSMapper);
  inherited TearDown;
end;

function TCustomTestSrcMap.ConvertJSModuleToString(El: TJSElement): string;
begin
  writeln('TCustomTestSrcMap.JSToStr ',GetObjName(El));
  JS_Writer.WriteJS(El);
  Result:=Pas2JSMapper.AsString;
end;

procedure TCustomTestSrcMap.CheckSrcMap(const aTitle: string;
  const JSLines: array of string);
type
  TMarker = record
    Name: string;
    PasLine: integer; // 1-based
    PasColMin,PasColMax: integer; // 0-based
  end;
  PMarker = ^TMarker;
var
  Markers: array of TMarker;
  PasSrc: TStringList;

  function IndexOfMarker(const aName: String): integer;
  var
    i: Integer;
  begin
    for i:=0 to length(Markers)-1 do
      if CompareText(Markers[i].Name,aName)=0 then
        exit(i);
    Result:=-1;
  end;

  procedure AddMarker(const aName: String; PasLine, PasColMin, PasColMax: integer);
  var
    i, l: Integer;
    p: PMarker;
  begin
    if IndexOfMarker(aName)>0 then
      begin
      writeln('AddMarker duplicate marker "',aName,'"');
      for i:=1 to PasLine do
        writeln(PasSrc[i-1]);
      Fail('duplicate marker "'+aName+'"');
      end;
    l:=length(Markers);
    SetLength(Markers,l+1);
    p:=@Markers[l];
    p^.Name:=aName;
    p^.PasLine:=PasLine;
    p^.PasColMin:=PasColMin;
    p^.PasColMax:=PasColMax;
  end;

  procedure JSMarkerError(Line, Col: integer; Msg: string);
  var
    i: Integer;
  begin
    for i:=0 to Line-1 do
      writeln(JSSource[i]);
    for i:=1 to Col do write('-');
    writeln('^');
    Fail(Msg+' at '+IntToStr(Line)+','+IntToStr(Col));
  end;

var
  i, j, ColMin, ColMax: integer;
  Line, aName, SegFile, ActLine: String;
  p, StartP, ActP: PChar;
  m: PMarker;
  aSeg: TSourceMapSegment;
begin
  {$IFDEF VerbosePas2JS}
  writeln('TCustomTestSrcMap.CheckSrcMap ',aTitle);
  {for i:=0 to SrcMap.Count-1 do
    begin
    write('TCustomTestSrcMap.CheckSrcMap i=',i,' Gen=',
      SrcMap[i].GeneratedLine,',',SrcMap[i].GeneratedColumn);
    write(' Src=');
    if SrcMap[i].SrcFileIndex>0 then
      write(SrcMap.SourceFiles[SrcMap[i].SrcFileIndex],',');
    writeln(SrcMap[i].SrcLine,',',SrcMap[i].SrcColumn);
    end;}
  for i:=1 to JSSource.Count do
    WriteSrcMapLine(i);
  WriteSources(Filename,1,1);
  writeln('......012345678901234567890123456789012345678901234567890123456789');
  {$ENDIF}
  if Low(JSLines)<>0 then
    {%H-}Fail('inconsistency');
  AssertEquals('expected JavaScript lines',High(JSLines)+1,JSSource.Count);

  // collect markers in Pascal
  PasSrc:=TStringList.Create;
  try
    PasSrc.Text:=Engine.Source;
    for i:=1 to PasSrc.Count do
      begin
      Line:=PasSrc[i-1];
      p:=PChar(Line);
      repeat
        case p^ of
        #0: break;
        '(':
          if (p[1]='*') and (p[2] in ['a'..'z','A'..'Z','_']) then
            begin
            ColMin:=p-PChar(Line);
            inc(p,2);
            StartP:=p;
            while p^ in ['a'..'z','A'..'Z','0'..'9','_'] do inc(p);
            aName:=copy(Line,StartP-PChar(Line)+1,p-StartP);
            if (p^<>'*') or (p[1]<>')') then
              begin
              for j:=1 to i do
                writeln(PasSrc[j-1]);
              Fail('missing closing bracket of Pascal marker at '+IntToStr(i)+','+IntToStr(p-PChar(Line)));
              end;
            inc(p,2);
            ColMax:=p-PChar(Line);
            AddMarker(aName,i,ColMin,ColMax);
            continue;
            end;
        end;
        inc(p);
      until false;
      end;

    // check JavaScript markers
    for i:=1 to JSSource.Count do
      begin
      ActLine:=JSSource[i-1];
      if i>High(JSLines)+1 then
        begin
        writeln('TCustomTestSrcMap.CheckSrcMap unexpected JS line ',i,': ',ActLine);
        Fail('created JS has more lines than expected JS');
        end;
      ActP:=PChar(ActLine);
      Line:=JSLines[i-1];
      p:=PChar(Line);
      repeat
        case p^ of
        #0: break;
        '(':
          if (p[1]='*') and (p[2] in ['a'..'z','A'..'Z','_']) then
            begin
            ColMin:=ActP-PChar(ActLine);
            inc(p,2);
            StartP:=p;
            while p^ in ['a'..'z','A'..'Z','0'..'9','_'] do inc(p);
            aName:=copy(Line,StartP-PChar(Line)+1,p-StartP);
            if (p^<>'*') or (p[1]<>')') then
              begin
              for j:=1 to i do
                writeln(JSSource[j-1]);
              Fail('missing closing bracket of JS marker at '+IntToStr(i)+','+IntToStr(ColMin));
              end;
            inc(p,2);
            j:=IndexOfMarker(aName);
            if j<0 then
              JSMarkerError(i,ColMin,'JS marker "'+aName+'" not found in Pascal');
            m:=@Markers[j];
            j:=SrcMap.IndexOfSegmentAt(i,ColMin);
            if j<0 then
              JSMarkerError(i,ColMin,'JS marker "'+aName+'" has no segment in SrcMap');
            aSeg:=SrcMap[j];
            SegFile:=SrcMap.SourceFiles[aSeg.SrcFileIndex];
            if SegFile<>Filename then
              JSMarkerError(i,ColMin,'JS marker "'+aName+'" maps to file "'+SegFile+'" instead of "'+Filename+'"');
            if aSeg.SrcLine<>m^.PasLine then
              JSMarkerError(i,ColMin,'JS marker "'+aName+'" maps to Pascal line "'+IntToStr(aSeg.SrcLine)+'" instead of "'+IntToStr(m^.PasLine)+'"');
            if (aSeg.SrcColumn<m^.PasColMin) or (aSeg.SrcColumn>m^.PasColMax) then
              JSMarkerError(i,ColMin,'JS marker "'+aName+'" maps to Pascal col "'+IntToStr(aSeg.SrcColumn)+'" instead of "'+IntToStr(m^.PasColMin)+'-'+IntToStr(m^.PasColMax)+'"');
            continue;
            end;
        end;
        if p^<>ActP^ then
          begin
          writeln('JavaScript: ');
          for j:=0 to i-1 do
            writeln(JSSource[j]);
          for j:=1 to P-PChar(Line) do write('-');
          writeln('^');
          writeln('Expected JS:<',Line,'>');
          AssertEquals('Expected JavaScript differs',p^,ActP^);
          end;
        inc(p);
        inc(ActP);
      until false;
      end;
  finally
    PasSrc.Free;
  end;
end;

procedure TCustomTestSrcMap.WriteSrcMapLine(GeneratedLine: integer);
var
  JS, Origins: String;
begin
  JS:=JSSource[GeneratedLine-1];
  DebugSrcMapLine(GeneratedLine,JS,SrcMap,Origins);
  writeln(JS);
  writeln(Origins);
end;

{ TTestSrcMap }

procedure TTestSrcMap.TestEmptyProgram;
begin
  StartProgram(false);
  Add('(*b*)begin');
  ConvertProgram;
  CheckSrcMap('TestEmptyProgram',[
  'rtl.module("program", [], function () {',
  '  var $mod = this;',
  '(*b*)  $mod.$main = function () {',
  '  };',
  '});']);
end;

procedure TTestSrcMap.TestEmptyUnit;
begin
  StartUnit(false);
  Add([
  'interface',
  'implementation'
  ]);
  ConvertUnit;
  CheckSrcMap('TestEmptyUnit',[
  'rtl.module("Test1", [], function () {',
  '  var $mod = this;',
  '});']);
end;

procedure TTestSrcMap.TestIf;
begin
  StartProgram(false);
  Add([
  'var (*i*)i: longint;',
  'begin',
  '  if true then',
  '    (*a*)i:=(*b*)1234 (*c*)+ (*d*)2222',
  '  else',
  '    i:=3456;']);
  ConvertProgram;
  CheckSrcMap('TestIf',[
  'rtl.module("program", [], function () {',
  '  var $mod = this;',
  '  this.(*i*)i = 0;',
  '  $mod.$main = function () {',
  '    if (true) {',
  '      (*a*)$mod.i = (*b*)1234 (*c*)+ (*d*)2222}',
  '     else $mod.i = 3456;',
  '  };',
  '});']);
end;

procedure TTestSrcMap.TestIfBegin;
begin
  StartProgram(false);
  Add([
  'var',
  '  (*E*)E, (*P*)P: String;',
  'begin',
  '  (*E2*)E:=(*bla*)''bla'';',
  '  (*if1*)if E=P then',
  '    begin',
  '    (*then*)E:=''active'';',
  '    end',
  '  else',
  '    begin',
  '    (*else*)E:=''inactive'';',
  '    end;']);
  ConvertProgram;
  CheckSrcMap('TestIfBegin',[
  'rtl.module("program", [], function () {',
  '  var $mod = this;',
  '  this.(*E*)E = "";',
  '  this.(*P*)P = "";',
  '  $mod.$main = function () {',
  '(*E2*)    $mod.E = (*bla*)"bla";(*bla*)',
  '    (*if1*)if ($mod.E === $mod.P) {(*if1*)',
  '(*then*)      $mod.E = "active";',
  '    } else {',
  '(*else*)      $mod.E = "inactive";',
  '    };',
  '  };',
  '});']);
end;

procedure TTestSrcMap.TestForConstRange;
begin
  StartProgram(false);
  Add([
  'var Runner, i: longint;',
  'begin',
  '  (*for*)for (*r*)Runner := (*start*)1000 to (*end*)3000 do',
  '    (*inc*)inc(i);']);
  ConvertProgram;
  CheckSrcMap('TestForConstRange',[
  'rtl.module("program", [], function () {',
  '  var $mod = this;',
  '  this.Runner = 0;',
  '  this.i = 0;',
  '  $mod.$main = function () {',
  '(*for*)    for ((*r*)$mod.Runner = (*start*)1000; (*r*)$mod.Runner (*end*)<= 3000; (*r*)$mod.Runner++) $mod.i (*inc*)+= 1;',
  '  };',
  '});'
  ]);
end;

procedure TTestSrcMap.TestFunction;
begin
  StartProgram(false);
  Add([
  'function DoIt(i: longint): longint; forward;',
  'const p = 3;',
  'function (*ResultInit*)DoIt(*DoIt*)(i: longint): longint;',
  'var Runner, j: longint;',
  'begin',
  '  j:=0;',
  '  (*for*)for (*r*)Runner := (*start*)p to (*end*)j do',
  '    (*inc*)inc(j);',
  '  Result:=j;',
  'end;',
  'begin',
  '  (*CallDoIt*)DoIt(2);']);
  ConvertProgram;
  CheckSrcMap('TestFunction',[
  'rtl.module("program", [], function () {',
  '  var $mod = this;',
  '  this.p = 3;',
  '(*DoIt*)  this.DoIt = function (i) {',
  '(*ResultInit*)    var Result = 0;',
  '    var Runner = 0;',
  '    var j = 0;',
  '    j = 0;',
  '    for (var $l = 3, $end = j; $l <= $end; $l++) {',
  '      Runner = $l;',
  '      j += 1;',
  '    };',
  '    Result = j;',
  '    return Result;',
  '  };',
  '  $mod.$main = function () {',
  '(*CallDoIt*)    $mod.DoIt(2);',
  '  };',
  '});'
  ]);
end;

procedure TTestSrcMap.TestExternalObjCall;
begin
  StartProgram(false);
  Add([
  '{$modeswitch externalclass}',
  'type',
  '  TJSConsole = class external name ''Console''',
  '  Public',
  '    procedure log(Obj1 : JSValue); varargs;',
  '  end;',
  'var console : TJSConsole; external name ''window.console'';',
  '  xhrstatus: longint;',
  'begin',
  '  (*w*)console(*log*).log     (''state'');',
  '  if xhrstatus=200 then',
  '    begin',
  '      xhrstatus:=3;',
  '      xhrstatus:=4;',
  '    end;']);
  ConvertProgram;
  CheckSrcMap('TestExternalObjCall',[
  'rtl.module("program", [], function () {',
  '  var $mod = this;',
  '  this.xhrstatus = 0;',
  '  $mod.$main = function () {',
  '    (*w*)window.console(*log*).log("state");',
  '    if ($mod.xhrstatus === 200) {',
  '      $mod.xhrstatus = 3;',
  '      $mod.xhrstatus = 4;',
  '    };',
  '  };',
  '});'
  ]);
end;

procedure TTestSrcMap.TestBracketAccessor;
begin
  StartProgram(false);
  Add([
  '{$modeswitch externalclass}',
  'type',
  '  TJSObject = class external name ''Object''',
  '  private',
  '    function GetProperties(Name: String): JSValue; external name ''[]'';',
  '  Public',
  '    property Properties[Name: string]: JSValue read GetProperties;',
  '  end;',
  'var Obj : TJSObject;',
  '  j: JSValue;',
  'begin',
  '  (*j*)j:=(*Obj*)Obj.Properties[(*bracket*)''state''];',
  '  ']);
  ConvertProgram;
  CheckSrcMap('TestExternalObjCall',[
  'rtl.module("program", [], function () {',
  '  var $mod = this;',
  '  this.Obj = null;',
  '  this.j = undefined;',
  '  $mod.$main = function () {',
  '(*j*)    $mod.j = (*Obj*)$mod.Obj(*bracket*)["state"];',
  '  };',
  '});']);
end;

Initialization
  RegisterTests([TTestSrcMap]);

end.

