{$mode objfpc}
{$modeswitch objectivec2}

unit uobjc41;

interface

type
  NSSubject = objcclass(NSObject)
  end;

  MyCategory = objccategory(NSObject)
    procedure key; message 'key';
  end;

implementation

procedure MyCategory.key;
begin
end;

begin
end.
