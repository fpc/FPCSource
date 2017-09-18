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
    procedure Test;
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
  JS, Origins, Addition: String;
  GeneratedCol: integer; // 0-based
  i, diff, GenColStep: Integer;
  aSeg: TSourceMapSegment;
begin
  JS:=JSSource[GeneratedLine-1];
  Origins:='';
  GeneratedCol:=0;// 0-based
  i:=SrcMap.IndexOfSegmentAt(GeneratedLine,GeneratedCol);
  aSeg:=nil;
  if i<0 then
    begin
    // no segment at line start
    i:=0;
    if (i=SrcMap.Count) then
      aSeg:=nil
    else
      aSeg:=SrcMap[i];
    if (aSeg=nil) or (aSeg.GeneratedLine>GeneratedLine) then
      begin
      // no segment in line
      for i:=1 to length(JS) do Origins:=Origins+'?';
      writeln(JS);
      writeln(Origins);
      exit;
      end
    else
      begin
      // show "?" til start of first segment
      for i:=1 to aSeg.GeneratedColumn do Origins:=Origins+'?';
      end;
    end
  else
    aSeg:=SrcMap[i];

  repeat
    Addition:='';
    if (aSeg.GeneratedLine=GeneratedLine) and (aSeg.GeneratedColumn=GeneratedCol) then
      begin
      // segment starts here  -> write "|line,col"
      Addition:='|'+IntToStr(aSeg.SrcLine)+','+IntToStr(aSeg.SrcColumn);
      Origins:=Origins+Addition;
      end;
    inc(i);
    // skip segments at same GeneratedLine/Col
    while (i<SrcMap.Count) do
      begin
      aSeg:=SrcMap[i];
      if (aSeg.GeneratedLine=GeneratedLine) and (aSeg.GeneratedColumn=GeneratedCol) then
        inc(i)
      else
        break;
      end;
    if (i=SrcMap.Count) then
      aSeg:=nil
    else
      aSeg:=SrcMap[i];
    if (aSeg=nil) or (aSeg.GeneratedLine>GeneratedLine) then
      begin
      // in the last segment
      while length(Origins)<length(JS) do
        Origins:=Origins+'.';
      writeln(JS);
      writeln(Origins);
      exit;
      end;
    // there is another segment in this line
    // -> align JS and Origins
    GenColStep:=aSeg.GeneratedColumn-GeneratedCol;
    diff:=GenColStep-length(Addition);
    if diff<0 then
      // for example:
      //  JS:       if(~~e)~~~{
      //  Origins:  |12,3|12,5|12,7
      Insert(StringOfChar('~',-diff),JS,length(Origins)-length(Addition)+1+GenColStep)
    else
      while diff>0 do
        begin
        Origins:=Origins+'.';
        dec(diff);
        end;
    GeneratedCol:=aSeg.GeneratedColumn;
  until false;
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

procedure TTestSrcMap.Test;
begin

end;

Initialization
  RegisterTests([TTestSrcMap]);

end.

