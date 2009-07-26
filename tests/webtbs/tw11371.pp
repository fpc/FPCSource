program tw11371;

{$mode delphi}{$H+}

uses
  Variants;

procedure Test1(const s: string);
begin
  // nothing
end;

procedure Test2(const s: UTF8String);
begin
  // nothing
end;

var
  V: Variant;

begin
  V := 'Test';
  Test1(V);
  Test2(V);
end.

