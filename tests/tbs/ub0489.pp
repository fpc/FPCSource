{$mode delphi}
{$inline on}

unit ub0489;

interface

function test(b:integer;const x;c:integer):integer inline;

implementation

uses
  ub0489b;

function test(b:integer;const x;c:integer):integer inline;
begin
  result:=fpwrite(b,x,c);
end;

end.
