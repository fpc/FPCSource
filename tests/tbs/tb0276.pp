{ Old file: tbs0327.pp }
{  }

{$ifdef fpc}{$mode delphi}{$endif}
unit tb0276;
interface

type
  tc=class
    procedure l(i:integer);overload;
    procedure l(s:string);overload;
  end;

    procedure l2(i:integer);overload;
    procedure l2(s:string);overload;

implementation

    procedure l3(i:integer);forward;overload;
    procedure l3(s:string);forward;overload;

procedure tc.l(i:integer);
begin
end;

procedure tc.l(s:string);
begin
end;

procedure l2(i:integer);
begin
end;

procedure l2(s:string);
begin
end;

procedure l3(i:integer);overload;
begin
end;

procedure l3(s:string);
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
