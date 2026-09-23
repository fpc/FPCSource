{ %FAIL }
{ %EXPECTMSGS=3084 }

{ an assignment operator without a parameter is refused ("Impossible operator
  overload"), not a crash of the compiler }
program tb0343;

{$mode objfpc}

type
  TRec = record
    Value: Int64;
  end;

operator := () Res: TRec;
begin
  Res.Value:=0;
end;

begin
end.
