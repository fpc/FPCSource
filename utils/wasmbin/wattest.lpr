program wattest;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, watparser, watscanner, wasmmodule;

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

procedure Run(const fn: string);
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
    //Traverse(p);
    m := TWasmModule.Create;
    if not ParseModule(p, m, err) then
      writeln('Error: ', err);

  finally
    p.Free;
    st.Free;
  end;
end;

var
  fn : string;
begin
  if ParamCount=0 then begin
    writeln('please sepcify the input .wat file');
    exit;
  end;
  fn:=ParamStr(1);
  if not FileExists(fn) then begin
    writeln('file doesn''t exist: ', fn);
    exit;
  end;
  Run(fn);
end.

