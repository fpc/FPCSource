program example74;

uses baseunix;

Var
  P : PGrpArr;
  C,R,I : Cint;
  
begin
  C:=5;
  GetMem(P,Sizeof(tgid)*C);
  FillChar(P^,Sizeof(tgid)*C,0);
  R:=fpGetGroups(C,P^);
  If (R>0) then
    begin
    For I:=0 to R-1 do
       Writeln('Group id :',P^[I]);
    end;
end.