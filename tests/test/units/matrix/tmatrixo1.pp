{ Unit matrixo: the object version of the vector and matrix types, with
  constructors that work with new(p,init(...)). }
program tmatrixo1;

{$mode objfpc}

uses
  matrixo;

var
  lVec: ^Tvector3_single;
  lMat: ^Tmatrix2_double;
  lSum: Tvector3_single;

begin
  new(lVec,init(1,2,3));
  if (lVec^.data[0]<>1) or (lVec^.data[1]<>2) or (lVec^.data[2]<>3) then
    begin
    Writeln('Error: new(p,init(1,2,3)) of Tvector3_single');
    Halt(1);
    end;
  lSum:=lVec^+lVec^;
  if lSum.data[2]<>6 then
    begin
    Writeln('Error: Tvector3_single addition');
    Halt(2);
    end;
  dispose(lVec);
  new(lMat,init_identity);
  if (lMat^.data[0,0]<>1) or (lMat^.data[0,1]<>0) or (lMat^.determinant<>1) then
    begin
    Writeln('Error: new(p,init_identity) of Tmatrix2_double');
    Halt(3);
    end;
  dispose(lMat);
  Writeln('ok');
end.
