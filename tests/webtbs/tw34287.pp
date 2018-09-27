{ %NORUN }

program tw34287;

{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}

uses
  Classes,
  uw34287a,
  uw34287b;

var
  fooa: uw34287a.TFoo;
  foob: uw34287b.TFoo;
begin
  fooa := uw34287a.TFoo.Create(nil);
  fooa.Bar('');
  foob := uw34287b.TFoo.Create(nil);
  foob.Bar('');
end.
