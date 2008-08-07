unit exutils;

interface

{$mode objfpc}

uses
  SysUtils;

procedure printf(const msg: string; const args: array of const);
procedure printfn(const msg: string; const args: array of const);

implementation

procedure printf(const msg: string; const args: array of const);
begin
  write(Format(msg, args));
end;

procedure printfn(const msg: string; const args: array of const);
begin
  writeln(Format(msg, args));
end;

end.