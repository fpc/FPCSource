
program calculate_offset;

{$T-}
{ In T- mode, i.e. not in typed pointer mode,
  the difference between two pointers of the same type
  is divided by number of bytes between the two pointers }

type
   TRecord = record
     First: word;
     Second: qword;
   end;
   PRecord = ^TRecord;

var
   res,res2 : PtrInt;
   Rec : TRecord;
begin
   Res := @(PRecord(@Rec)^.Second) - @(PRecord(@Rec)^.First);
   writeln('Offset of Second field inside TRecord is ',Res);
   if (Res<2) then
     begin
       writeln('Offset of second field is smaller than size of first field');
       halt(1);
     end;
   Res2 := @(PRecord(nil)^.Second) - @(PRecord(nil)^.First);
   writeln('Offset of Second field inside TRecord  using constant pointers is ',Res2);
   if (Res2<2) then
     begin
       writeln('Offset of second field is smaller than size of first field');
       halt(2);
     end;
   if (Res2<>Res) then
     begin
       writeln('Inconsistent result for offset of second field');
       halt(3);
     end;
end.
