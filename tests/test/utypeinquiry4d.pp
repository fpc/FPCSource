unit utypeinquiry4d;

{ mode fpc does not have the modeswitch by default }
{$mode fpc}
{$modeswitch typeinquiry}

interface

var
  w: word;
  FpcWithSwitchVar: type of w;

const
  FpcWithSwitchSize = SizeOf(type of w);

implementation

end.
