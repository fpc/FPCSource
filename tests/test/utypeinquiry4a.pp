unit utypeinquiry4a;

{ mode delphi does not have the modeswitch by default }
{$mode delphi}
{$modeswitch typeinquiry}

interface

var
  w: word = 0;
  DelphiVar: type of w;

const
  DelphiSize = SizeOf(type of w);

implementation

end.
