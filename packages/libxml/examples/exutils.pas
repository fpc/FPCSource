unit exutils;

interface

{$mode objfpc}

uses
  ctypes,
  xml2,
  SysUtils;

procedure docdump(doc: xmlDocPtr);
procedure printf(const msg: string; const args: array of const);
procedure printf(const msg: string);
procedure printfn(const msg: string; const args: array of const);
procedure printfn(const msg: string);

implementation

procedure docdump(doc: xmlDocPtr);
var
  mem: xmlCharPtr;
  size: cint;
begin
  mem := nil;
  xmlDocDumpMemory(doc, mem, size);
  writeln(pchar(mem));
  xmlFree(mem);
end;

procedure printf(const msg: string; const args: array of const);
begin
  write(Format(msg, args));
end;

procedure printf(const msg: string);
begin
  write(msg);
end;

procedure printfn(const msg: string; const args: array of const);
begin
  writeln(Format(msg, args));
end;

procedure printfn(const msg: string);
begin
  writeln(msg);
end;

end.
