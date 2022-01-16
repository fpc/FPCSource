type
     PStreamRec= ^TStreamRec;

     TStreamRec = Packed Record
       ObjType : byte;
       Next : PStreamRec;
     end;

const
   BaseRec : PStreamRec= nil;

   RType1 : TStreamRec = (
    ObjType : 79
   );
   RType2 : TStreamRec = (
    objtype : 80
   );


procedure RegisterType(var R : TStreamRec);
var
  P : PStreamRec;

begin
  P := BaseRec;
  while (P <> nil) and (P^.Objtype <> R.ObjType) do
    P:=P^.Next;
  if not assigned(P) then
    begin
      R.Next:=BaseRec;
      BaseRec:=@R;
    end;
  { nothing to do here
  else
    P:=@R; }
end;

begin
  RegisterType(Rtype1);
  RegisterType(RType2);
end.

