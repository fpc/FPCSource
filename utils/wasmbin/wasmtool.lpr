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

const
  ACT_EXPORTRENAME = 'exprotrename';
  ACT_SYMBOLFLAG   = 'symbolflag';

procedure PrintHelp;
begin
  writeln('wasmtool [options] .wasm file...');
  writeln();
  writeln('options:');
  writeln('  --exportrename @inputfile');
  writeln('  --symbolflag   @inputfile');
end;

type

  { TToolActions }

  TToolActions = class(TObject)
    action   : string; // action to take place
    paramsFn : string; // input file name
    constructor Create(const aaction, afilename: string);
  end;

procedure ProcessParams(acts: TList; const inputFn: string);
var
  i  : integer;
  ta : TToolActions;
begin
  for i:=0 to acts.Count-1 do begin
    ta := TToolActions(acts[i]);
    writeln('i=',i);
    if ta.action = ACT_EXPORTRENAME then begin
      ExportRename(inputFn, ta.paramsFn);
    end else if ta.action = ACT_SYMBOLFLAG then begin
      ChangeSymbolFlag(inputFn, ta.paramsFn);
    end;
  end;
end;

var
  acts: TList = nil;
  inputFn: string = '';

procedure ParseParams;
var
  i : integer;
  s : string;
  ls : string;
  fn : string;
begin
  i:=1;
  while i<=ParamCount do begin
    s := ParamStr(i);
    ls := AnsiLowerCase(s);
    inc(i);
    if Pos('--',ls)=1 then begin
      inc(i);
      if i<=ParamCount then
        fn:=ParamStr(i)
      else
        fn := '';
      if fn <> '' then
        acts.Add( TToolActions.Create(ls, fn));
    end else begin
      if inputFn ='' then
        inputFn:=s;
    end;
  end;
end;

{ TToolActions }

constructor TToolActions.Create(const aaction, afilename: string);
begin
  action := aaction;
  inputFn := afilename;
end;

var
  i : integer;
begin
  if ParamCount=0 then begin
    PrintHelp;
    Exit;
  end;

  try
    acts := TList.Create;
    try
      ParseParams;
      ProcessParams(acts, inputFn);
    finally
      for i:=0 to acts.Count-1 do
        TObject(acts[i]).Free;
      acts.Free;
    end;
  except
    on e:exception do begin
      writeln('error: ', e.Message);
      ExitCode:=1;
    end;
  end;
end.

