{ Source provided for Free Pascal Bug Report 3113 }
{ Submitted by "Michalis Kamburelis" on  2004-05-23 }
{ e-mail: michalis@camelot.homedns.org }
type
  TVector3 = array[0..2]of Integer;

procedure P(const A:array of TVector3);
var i:Integer;
begin
 Writeln(High(A));

 if High(A)<>0 then
   begin
     writeln('Error!');
     halt(1);
   end;

 for i:=0 to High(A) do
  Writeln('  ', A[i][0], ' ', A[i][1], ' ', A[i][2]);
end;

const A1:TVector3 = (1, 2, 3);
begin
 P(A1);
 { When changed to P([A1]), works OK. }
end.
