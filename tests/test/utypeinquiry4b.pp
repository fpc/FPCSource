unit utypeinquiry4b;

{ mode delphiunicode does not have the modeswitch by default }
{$mode delphiunicode}
{$modeswitch typeinquiry}

interface

var
  d: longword = 0;
  DelphiUnicodeVar: type of d;

const
  DelphiUnicodeSize = SizeOf(type of d);

implementation

end.
