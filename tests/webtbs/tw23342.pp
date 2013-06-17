uses
  classes;

procedure foo(Shift: TShiftState);
var
  ssMultiSelect: TShiftStateEnum;
  ATest: Boolean;
begin
  ssMultiSelect := ssCtrl;
  // ATest := (Shift = [ssLeft, ssMultiSelect]); // compiles
  // ATest := ATest or (Shift = [ssMultiSelect]); // compiles
  ATest := (Shift = [ssLeft, ssMultiSelect]) or (Shift = [ssMultiSelect]); // fatal internal error 200203302 with -O2 or -O3
end;

begin
end.

