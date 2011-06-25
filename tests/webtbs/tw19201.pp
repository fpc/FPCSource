{ %cpu=i386 }
{ %opt=-Cppentium3 -Cr }
program testcmov;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;
var cur,i,resultid, lastNeighbour,firstNeighbour:integer;
  OldPositions2: array of Integer;
begin
 for cur:=lastNeighbour+1 to firstNeighbour do
   if abs(OldPositions2[cur]-OldPositions2[i]) < abs(OldPositions2[resultId]-OldPositions2[i])  then
     resultid:=cur;
end.

