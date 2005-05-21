{$ifdef fpc}{$mode objfpc}{$endif}

type

TGraphicControl = class
end;

TButton = class
end;

TBitBtn = class(TButton)
private
published
end;

TSpeedButton = class(TGraphicControl)
published
end;

TMyButton = class(TBitBtn);

const MyConst = 1;

begin
end.
