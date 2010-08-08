{ %interactive }

{ see tw13480d.pp for test instructions }

{$mode objfpc}

unit tw13840b;

interface

uses
  tw13840a;

type
  tb = class(ta)
  end;

implementation
{
procedure tb.test;
begin
  writeln('tb.test');
  inherited test;
end;
}
end.
