{ %opt=-gh }

program tw31675;

{$mode objfpc}{$H+}

uses
  Classes,
  {$if declared(useheaptrace)}
  uw31675,
  {$endif}
  SysUtils
  ;

var
  i: Boolean = false;
begin
{$if declared(foo)}
  i := true;
{$endif}
  if not i then
    Halt(1);
  WriteLn('ok');
end.

