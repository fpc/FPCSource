type
  TShiftStateEnum = (ssShift, ssAlt, ssCtrl,
    ssLeft, ssRight, ssMiddle, ssDouble,
    // Extra additions
    ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum,
    ssScroll,ssTriple,ssQuad);

{$packset 1}
  TShiftState = set of TShiftStateEnum;
{$packset default}

var
  s: tshiftstate;
  ss: tshiftstateenum;
begin
  s := [];
  ss:=ssShift;
  include(s,ss);
  include(s,ssSuper);
  if not(ssShift in s) or
     not(ssSuper in s) then
    halt(1);
  if not(ss in s) then
    halt(2);
end.

