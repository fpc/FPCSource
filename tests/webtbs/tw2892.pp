{ Source provided for Free Pascal Bug Report 2892 }
{ Submitted by "Eric Grange" on  2004-01-12 }
{ e-mail: egrange@glscene.org }

{$mode delphi}

type
   TAffineVector = array [0..2] of Single;
   TVector = array [0..3] of Single;

function VectorMake(const v : TAffineVector; w : Single = 0) : TVector; overload;
begin
end;

function VectorMake(const x, y, z: Single; w : Single = 0) : TVector; overload;
begin
end;

var
   avec : TAffineVector;
   vec : TVector;
begin
   vec:=VectorMake(avec);
end.
