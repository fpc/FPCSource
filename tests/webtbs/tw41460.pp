program calculate_offset;

{$T-}

type
   TRecord = record
     First: word;
     Second: word;
   end;
   PRecord = ^TRecord;

var
   res, res2, res3 : PtrInt;
   Rec : TRecord;

begin
{$ifndef SKIP_CONST_POINTER}
   Res := @(PRecord(nil)^.Second) - @(PRecord(nil)^.First);
   writeln('Offset of Second field inside TRecord is ',Res);
   if (Res<2) then
     begin
       writeln('Offset of second field is smaller than size of first field');
       halt(1);
     end;
{$else SKIP_CONST_POINTER}
   Res:=2;
{$endif SKIP_CONST_POINTER}
   Res2 := @(PRecord(@Rec)^.Second) - @(PRecord(@Rec)^.First);
   writeln('Offset of Second field inside TRecord is ',Res2);
   if (Res2<2) then
     begin
       writeln('Offset of second field is smaller than size of first field');
       halt(2);
     end;
   if (Res<>Res2) then
     begin
       writeln('Inconsistent results for Offset of second field between constant and non-constant pointers');
       halt(3);
     end;
   Res3 := @(Rec.Second) - @(Rec.First);
   if (Res3<2) then
     begin
       writeln('Offset of second field is smaller than size of first field');
       halt(4);
     end;
   if (Res<>Res3) then
     begin
       writeln('Inconsistent results for Offset of second field between constant and non-constant pointers');
       halt(5);
     end;
end.
