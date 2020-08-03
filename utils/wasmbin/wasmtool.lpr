program wasmtool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  { you can add units after this }
  Classes, SysUtils, wasmbin, lebutils,
  wasmbindebug, wasmlink, wasmlinkchange,
  wasmtoolutils;

procedure PrintHelp;
begin
  writeln('wasmtool [options] .wasm file...');
  writeln();
  writeln('options:');
  writeln('  --exportrename @inputfile');
  writeln('  --symbolflag @inputfile');
end;


var
  symfn : string;
begin
  if ParamCount=0 then begin
    PrintHelp;

    exit;
  end;
  symfn:='';
  if ParamCount>1 then symfn:=ParamStr(2);

  //ReadWasmFile(ParamStr(1));
  //ProcessWasmFile(ParamStr(1), symfn);
  ProcessWasmSection(ParamStr(1), symfn);
end.

