program calculate_offset;

{$T+}
{ In T+ mode, i.e. type pointer mode,
  the difference between two pointers of the same type
  is divided by the byte size of that type }
type
   TRecord = record
     First: word;
     Second: word;
   end;
   PRecord = ^TRecord;

var
   res, res_nil : PtrInt;
   Rec : TRecord;
begin
   Res := @(PRecord(@Rec)^.Second) - @(PRecord(@Rec)^.First);
   writeln('Number of word fields between Second and First is ',Res);
   if (Res<>1) then
     begin
       writeln('Error in typed pointer arithmetics');
       halt(1);
     end;
   Res_nil := @(PRecord(nil)^.Second) - @(PRecord(nil)^.First);
   writeln('Number of word fields between Second and First is ',Res_nil);
   if (Res_nil<>1) then
     begin
       writeln('Error in constant typed pointer arithmetics');
       halt(2);
     end;
end.
