Program test_operator;

{$mode fpc}

type
    Vector = record
        X,Y,Z : extended;
        end;
    Matrix = array [1..4,1..4] of extended;

Const
  IDENTITYMATRIX : Matrix =
  ( (1,0,0,0),
    (0,1,0,0),
    (0,0,1,0),
    (0,0,0,1));
{...}

function NewVector (ax,ay,az : extended) : Vector;
begin
  NewVector.X:=ax;
  NewVector.Y:=ay;
  NewVector.Z:=az;
end;

operator * (V : Vector;Value : extended) Result : Vector;
  begin
    Result.X:=Result.X*Value;
    Result.Y:=Result.Y*Value;
    Result.Z:=Result.Z*Value;
  end;
{...}
operator * (Value : extended;V : Vector) Result : Vector;
  begin
    Result.X:=Result.X*Value;
    Result.Y:=Result.Y*Value;
    Result.Z:=Result.Z*Value;
  end;
{...}


operator * (M : Matrix;Value : extended) Result : Matrix;
 var i,j : longint;
  begin
    for i:=1 to 4 do
      for j:=1 to 4 do
        Result[i,j]:=M[i,j]*Value;
  end;
{...}
operator * (Value : extended;M : Matrix) Result : Matrix;
 var i,j : longint;
  begin
    for i:=1 to 4 do
      for j:=1 to 4 do
        Result[i,j]:=M[i,j]*Value;
  end;
{...}

var
   V1, V2 : Vector;
   M1, M2 : Matrix;

begin
     V1 := NewVector (1,1,1);
     V2 := V1 * 2;
     { Everything ok }


     M2 := IDENTITYMATRIX;
     M1 := M2 * 2;
     M1 := IDENTITYMATRIX * 2;
     M2 := IDENTITYMATRIX * 4;
     { Error: Incompatible types: got "E3MATRIX" expected "LONGINT"  in both rows. This doesn't happen if I use 2.0 and 4.0 values. }


     {...}
end.
