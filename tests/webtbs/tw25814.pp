{ %norun }
{ %recompile=-dchangeuses }

unit tw25814;

{$mode delphi}

interface

uses {$ifdef changeuses}sysutils,{$endif} classes, uw25814;

type
  test = class
  function xyz(b: string): string;
  end;

implementation

function test.xyz(b: string): string;
begin
 result:='';
end;

end.

