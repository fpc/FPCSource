unit Assertions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

procedure AssertTrue(v1 : boolean);
procedure AssertEquals(v1,v2 : string); overload;
procedure AssertEquals(v1,v2 : integer); overload;

implementation

procedure AssertTrue(v1: boolean);
begin
  if not v1 then halt(1);
end;

procedure AssertEquals(v1, v2: string);
begin
  AssertTrue(v1=v2);
end;

procedure AssertEquals(v1, v2: integer);
begin
  AssertTrue(v1=v2);
end;

end.

