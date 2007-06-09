{$mode macpas}

program FatalError_200301231;
   type
     note_name_type = packed array[0..17] of string[2];
   var
     nn: note_name_type;
     s: string[ 80];
begin
   nn[1]:= 'x';
   s:=concat( 'y', nn[ 1]);
   if (s <> 'yx') then
     halt(1);
end.

