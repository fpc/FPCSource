{ %FAIL }
{ Old file: tbf0328.pp }
{  }

{$ifdef fpc}{$mode delphi}{$endif}

procedure k1(l:longint);
begin
end;

procedure k1(l:string);overload;
begin
end;

procedure k2(l:longint);overload;
begin
end;

procedure k2(l:string);
begin
end;


begin
end.
