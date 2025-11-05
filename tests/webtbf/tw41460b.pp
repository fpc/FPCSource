{ %FAIL }

program calculate_offset;

{$T+}
{ In T+ mode, i.e. type pointer mode,
  the difference between two pointers of the different types
  should generate an error }

type
   TRecord = record
     First: word;
     Second: qword;
   end;
   PRecord = ^TRecord;

var
   res: PtrInt;
   Rec : TRecord;
begin
   Res := @(PRecord(@Rec)^.Second) - @(PRecord(@Rec)^.First);
   writeln('Offset of Second field inside TRecord is ',Res);
   if (Res<2) then
     begin
       writeln('Offset of second field is smaller than size of first field');
       halt(1);
     end;
end.
