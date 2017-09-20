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
unit tcsrcmap;

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
    procedure CheckSrcMap(const aTitle: string); virtual;
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
    procedure TestFor;
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
  Result:=Pas2JSMapper.AsAnsistring;
end;

procedure TCustomTestSrcMap.CheckSrcMap(const aTitle: string);
{$IFDEF VerbosePas2JS}
var
  i: Integer;
{$ENDIF}
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
  writeln('......012345678901234567890123456789012345678901234567890123456789');
  WriteSources(Filename,1,1);
  {$ENDIF}
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
  Add('begin');
  ConvertProgram;
  CheckSrcMap('TestEmptyProgram');
end;

procedure TTestSrcMap.TestEmptyUnit;
begin
  StartUnit(false);
  Add([
  'interface',
  'implementation'
  ]);
  ConvertUnit;
  CheckSrcMap('TestEmptyUnit');
end;

procedure TTestSrcMap.TestIf;
begin
  StartProgram(false);
  Add([
  'var i: longint;',
  'begin',
  '  if true then',
  '    i:=1234 + 2222',
  '  else',
  '    i:=3456;']);
  ConvertProgram;
  CheckSrcMap('TestIf');
end;

procedure TTestSrcMap.TestIfBegin;
begin
  StartProgram(false);
  Add([
  'var',
  '  E, P: String;',
  'begin',
  '  E:=''bla'';',
  '  if E=P then',
  '    begin',
  '    E:=''active'';',
  '    end',
  '  else',
  '    begin',
  '    E:=''inactive'';',
  '    end;']);
  ConvertProgram;
  CheckSrcMap('TestIfBegin');
end;

procedure TTestSrcMap.TestFor;
begin
  StartProgram(false);
  Add([
  'var Runner, i: longint;',
  'begin',
  '  for Runner := 1000 + 2000 to 3000 do',
  '    inc(i);']);
  ConvertProgram;
  CheckSrcMap('TestEmptyProgram');
end;

procedure TTestSrcMap.TestFunction;
begin
  StartProgram(false);
  Add([
  'function DoIt(i: longint): longint; forward;',
  'const p = 3;',
  'function DoIt(i: longint): longint;',
  'var Runner, j: longint;',
  'begin',
  '  j:=0;',
  '  for Runner := p to j do',
  '    inc(j);',
  '  Result:=j;',
  'end;',
  'begin',
  '  DoIt(2);']);
  ConvertProgram;
  CheckSrcMap('TestFunction');
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
  '  console.log(''state'');',
  '  if xhrstatus=200 then',
  '    begin',
  '      xhrstatus:=3;',
  '      xhrstatus:=4;',
  '    end;']);
  ConvertProgram;
  CheckSrcMap('TestExternalObjCall');
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
  '  j:=Obj.Properties[''state''];',
  '  ']);
  ConvertProgram;
  CheckSrcMap('TestExternalObjCall');
end;

Initialization
  RegisterTests([TTestSrcMap]);

end.

