{ %FAIL }
{ %EXPECTMSGS=3084 }

{ a record's assignment class operator without a parameter is refused
  ("Impossible operator overload"), not a crash of the compiler }
program tb0345;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TRec = record
    Value: Int64;
    class operator := () Res: TRec;
  end;

class operator TRec.:= () Res: TRec;
begin
  Res.Value:=0;
end;

begin
end.
