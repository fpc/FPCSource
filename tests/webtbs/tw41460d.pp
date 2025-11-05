program calculate_offset;

{$T-}

type
   TRecord = record
     First: byte;
     Second: byte;
     Third: byte;
     Fourth: byte;
   end;
   PRecord = ^TRecord;

var
   res, res2, res3 : PtrInt;
   Rec : TRecord;

begin
   Res := @(PRecord(nil)^.Fourth) - @(PRecord(nil)^.First);
   writeln('Offset of Fourth field inside TRecord is ',Res);
   if (Res<3) then
     begin
       writeln('Offset of Fourth field is smaller than size of first field');
       halt(1);
     end;
   Res2 := @(PRecord(@Rec)^.Fourth) - @(PRecord(@Rec)^.First);
   writeln('Offset of Fourth field inside TRecord is ',Res2);
   if (Res2<3) then
     begin
       writeln('Offset of Fourth field is smaller than size of first field');
       halt(2);
     end;
   if (Res<>Res2) then
     begin
       writeln('Inconsistent results for Offset of Fourth field between constant and non-constant pointers');
       halt(3);
     end;
   Res3 := @(Rec.Fourth) - @(Rec.First);
   if (Res3<3) then
     begin
       writeln('Offset of Fourth field is smaller than size of first field');
       halt(4);
     end;
   if (Res<>Res3) then
     begin
       writeln('Inconsistent results for Offset of Fourth field between constant and non-constant pointers');
       halt(5);
     end;
end.
