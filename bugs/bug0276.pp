{$asmmode intel}
type
  trec = record
    ypos,
    xpos : longint;
   end;
var
  rec : tRec;
begin
  asm
     lea     edi, Rec
     cmp   byte ptr [edi+tRec.Xpos], true
     cmp   byte ptr [edi].trec.Xpos, true
  end;
end.
