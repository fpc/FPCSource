{ %skiptarget=aix }
{ %opt=-gs }
program ustabslink;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fgl;

type
  TMyClass = class end;
  TControlObjectSpecializedWithAVeryLongNameOfClass = class end;

  TMyType = TControlObjectSpecializedWithAVeryLongNameOfClass;  // Error

  TSpecControlInfo = specialize TFPGList<TMyType>;

begin
end.
