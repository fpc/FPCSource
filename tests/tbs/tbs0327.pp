{$ifdef fpc}{$mode delphi}{$endif}
unit tbs0327;
interface

type
  tc=class
    procedure l(i:integer);overload;
    procedure l(s:string);overload;
  end;

implementation

procedure tc.l(i:integer);
begin
end;

procedure tc.l(s:string);
begin
end;

procedure k(l:longint);overload;
begin
end;

procedure k(l:string);overload;
begin
end;

begin
end.
