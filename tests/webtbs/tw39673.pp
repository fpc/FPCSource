{ %NORUN }

program tw39673;

{$ifdef fpc}
{$mode delphi}
{$else}
{$APPTYPE CONSOLE}
{$endif}

uses
  Types,
  Classes,
  contnrs,
  generics.collections;

Type tsomeclass =class
                    function readarray:TObjectList;
end;

{ tsomeclass }

function tsomeclass.readarray: TObjectList;
begin
 result:=nil;
end;

begin
end.

