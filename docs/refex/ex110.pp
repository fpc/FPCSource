program Example110;

{ Program to demonstrate the Real2Double function. }

Var
  i : integer;
  R : Real48;
  D : Double;
  E : Extended;
  F : File of Real48;

begin
  Assign(F,'reals.dat');
  Reset(f);
  For I:=1 to 10 do
    begin
    Read(F,R);
    D:=Real2Double(R);
    Writeln('Real ',i,' : ',D);
    D:=R;
    Writeln('Real (direct to double)  ',i,' : ',D);
    E:=R;
    Writeln('Real (direct to Extended) ',i,' : ',E);
    end;
  Close(f);
end.