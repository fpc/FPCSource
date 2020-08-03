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
  ACT_EXPORTRENAME = 'exportrename';
  ACT_SYMBOLFLAG   = 'symbolflag';

  VERSION = '1.0';

procedure PrintHelp;
begin
  writeln('wasmtool [options] .wasm file...');
  writeln();
  writeln('version: ',VERSION);
  writeln;
  writeln('options:');
  writeln('  --exportrename @inputfile - renaming export names');
  writeln('  --symbolflag   @inputfile');
  writeln('  --verbose - enabling verbose mode');
end;

type

  { TToolActions }

  TToolActions = class(TObject)
    action   : string; // action to take place
    paramsFn : string; // input file name
    constructor Create(const aaction, afilename: string);
  end;

{ TToolActions }

constructor TToolActions.Create(const aaction, afilename: string);
begin
  inherited Create;
  action := aaction;
  paramsFn := afilename;
end;

procedure ProcessParams(acts: TList; const inputFn: string; doVerbose: Boolean);
var
  i  : integer;
  ta : TToolActions;
begin
  for i:=0 to acts.Count-1 do begin
    ta := TToolActions(acts[i]);
    if doVerbose then writeln('action: "',ta.action,'"');
    if ta.action = ACT_EXPORTRENAME then begin
      if doVerbose then begin
        writeln('  input:  ',inputFn);
        writeln('  params: ',ta.paramsFn);
      end;
      ExportRename(inputFn, ta.paramsFn, doVerbose);
    end else if ta.action = ACT_SYMBOLFLAG then begin
      ChangeSymbolFlag(inputFn, ta.paramsFn);
    end;
  end;
end;

var
  acts: TList = nil;
  inputFn: string = '';
  verbose: Boolean = false;

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

    if Pos('-', ls)=1 then begin
      if ls = '-v' then
        ls := '--verbose';
    end;

    if Pos('--',ls)=1 then begin
      ls := Copy(ls, 3, length(ls)-2);
      if (ls = 'verbose') then
        verbose := true
      else begin
        if i<=ParamCount then begin
          fn:=ParamStr(i);
          inc(i);
        end else
          fn := '';
        if fn <> '' then
          acts.Add( TToolActions.Create(ls, fn));
      end;
    end else begin
      if inputFn ='' then
        inputFn:=s;
    end;
  end;
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
      ProcessParams(acts, inputFn, verbose);
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

