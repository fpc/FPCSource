program bug11255;
{$mode objfpc}{$h+}

type
  TLCLPlatform = (
    lpCarbon
    );
    
  TLCLPlatforms = set of TLCLPlatform;

var
  WidgetSets: TLCLPlatforms;    

function DirNameToLCLPlatform: TLCLPlatform;
begin
  Result:=lpCarbon;
end;

begin
  WidgetSets := [DirNameToLCLPlatform];
  if widgetsets<>[lpcarbon] then
    halt(1);
end.
