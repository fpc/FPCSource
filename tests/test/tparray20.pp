{$mode macpas}
{$packrecords 4}

program FatalError_200608051;

   type
     String32 = string[ 32];
     String80 = string[ 80];
     TDS = record
         ZZStyleName: packed array[0..24] of String32;
       end;
     TDSunp = record
         ZZStyleName: array[0..24] of String32;
       end;
     PDS = ^TDS;

   var
     DV: PDS;

function StyleNameToSongStyleNum (TheSTYName80: String80): longint;
   var
     a: Integer;
begin
StyleNameToSongStyleNum := 1;
for A := 1 to 24 do
   if Pos(DV^.ZZStyleName[a], TheSTYName80) > 0 then
     begin
     StyleNameToSongStyleNum := A;
     exit;
     end;
end;

var
 i: integer;
begin
  new(dv);
  for i := 1 to 24 do
    tdsunp(dv^).ZZStyleName[i]:='MySong'+chr(i+ord('A'));
  if (StyleNameToSongStyleNum('MySongF') <> 5) then
    halt(1);
end.

