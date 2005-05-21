{ Source provided for Free Pascal Bug Report 3217 }
{ Submitted by "C Western" on  2004-07-18 }
{ e-mail: mftq75@dsl.pipex.com }
{$mode objfpc}
function TT(I:Integer):Double;
begin
  Result := (I+0.2)*(I+0.3)+(I+0.5);
end;

function T(I:Integer):Double;
begin
  Result := (I+0.2)*(I+0.3)*TT(I)+(I+0.5)*TT(I-1);
end;

function H(K, Kp: Integer): Double;
begin
  Result := T(K)+T(K+3);
end;

function M(K, Kp, P, PP: Integer): Double;
begin
  Result := H(K, Kp);
  if P = 0 then begin
    if PP <> 0 then
      Result := (Result + PP*H(K, -Kp))/Sqrt(2.0);
  end else if PP = 0 then
    Result := (Result + P*H(-K, Kp))/Sqrt(2.0)
  else
    Result := (Result + PP*H(K, -Kp)
                      + p*H(-K, Kp)
                      + PP*p*H(-K,-Kp)
               )/2;
end;

begin
  WriteLn(M(3,3,1,-1)-M(3,3,1,-1));
end.
