program wattest;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, watparser, watscanner, wasmmodule, wasmbinwriter;

procedure Traverse(p: TWatScanner);
begin
  while p.Next do begin
    write(p.token,' ', p.resText);
    if p.token = weInstr then
      write('; inst = $', IntToHex(p.instrCode,2))
    else if p.token = weError then begin
      writeln('offset = ',p.ofs,' ',p.resText);
      break;
    end;
    writeln;
  end;
end;

procedure WriteBin(const fndst: string; m: TWasmModule);
var
  f : TFileStream;
begin
  f := TFileStream.Create(fndst, fmCreate);
  try
    WriteModule(m, f);
  finally
    f.Free;
  end;
end;

procedure Run(const fn: string; const doTraverse: Boolean);
var
  st : TFileStream;
  s  : string;
  p  : TWatScanner;
  m  : TWasmModule;
  err : string;
begin
  st := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  p := TWatScanner.Create;
  try
    SetLength(s, st.Size);
    if length(s)>0 then st.Read(s[1], length(s));
    p.SetSource(s);
    if doTraverse then begin
      Traverse(p);
      Exit;
    end;
    m := TWasmModule.Create;
    try
      if not ParseModule(p, m, err) then
        writeln('Error: ', err)
      else
        WriteBin( ChangeFileExt(fn,'.wasm'), m);
    finally
      m.Free;
    end;
  finally
    p.Free;
    st.Free;
  end;
end;

var
  gFn      : string;
  gCommand : string = '';

procedure ParseParams;
var
  i : integer;
  s : string;
begin
  i:=1;
  while i<=ParamCount do begin
    s := ParamStr(i);
    if (s<>'') and (s[1]='-') then
      gCommand:=AnsiLowerCase(s)
    else
      gFn := s;
    inc(i);
  end;
end;

begin
  ParseParams;
  if (gFn='') then begin
    writeln('please sepcify the input .wat file');
    writeln('other use:');
    writeln(' -compile  %inpfn%');
    writeln(' -traverse %inpfn%');
    exit;
  end;
  if not FileExists(gFn) then begin
    writeln('file doesn''t exist: ', gFn);
    exit;
  end;
  try
    Run(gFn, gCommand = '-traverse');
  except
    on e: exception do
      writeln(e.message);
  end;
end.

