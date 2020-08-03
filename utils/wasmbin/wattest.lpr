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

procedure WriteBin(const fndst: string; m: TWasmModule; WriteReloc: Boolean);
var
  f : TFileStream;
begin
  f := TFileStream.Create(fndst, fmCreate);
  try
    Normalize(m);
    WriteModule(m, f, WriteReloc, WriteReloc);
  finally
    f.Free;
  end;
end;

procedure Run(const fn: string; const doTraverse: Boolean; doReloc: Boolean);
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
        WriteBin( ChangeFileExt(fn,'.wasm'), m, doReloc);
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
  gReloc   : Boolean = true;
  gCatch   : Boolean = false;

procedure ParseParams;
var
  i : integer;
  s : string;
  ls : string;
begin
  i:=1;
  while i<=ParamCount do begin
    s := ParamStr(i);
    if (s<>'') and (s[1]='-') then begin
      ls := AnsiLowerCase(s);
      if ls = '-noreloc' then gReloc := false
      else if ls = '-catch' then gCatch := true
      else gCommand:=ls;
    end else
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
    writeln('   -noreloc - prevents relocation information from being written');
    writeln(' -traverse %inpfn%');
    exit;
  end;
  if not FileExists(gFn) then begin
    writeln('file doesn''t exist: ', gFn);
    exit;
  end;

  if gCatch then
    try
      Run(gFn, gCommand = '-traverse', gReloc);
    except
      on e: exception do
        writeln(e.message);
    end
  else
    Run(gFn, gCommand = '-traverse', gReloc);

end.

