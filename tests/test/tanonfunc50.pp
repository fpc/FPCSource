{ %NORUN }

program tanonfunc50;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test combining multiple levels of anonymous methods and nested
  named procedures with variable capture.
  Causes an internal compiler error in Delphi 10.4 (RSP-21518) }

type
  TProc = reference to procedure;

procedure CallProc(AProc: TProc);
begin
  AProc();
end;

procedure OuterProc;
begin
  CallProc(
    procedure

      procedure NestedProc;
      var
        x, y, z: Integer;
      begin
        x := 0;
        y := 1;
        CallProc(
          procedure
          begin
            x := 2;
            z := 3;
          end);
      end;

    begin
    end);
end;

begin
  OuterProc;
end.


