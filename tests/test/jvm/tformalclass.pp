program tformalclass;

{$macro on}
{$ifdef java}
{$define sdkunit:=jdk15}
{$else}
{$define sdkunit:=androidr14}
{$endif}

uses
  sdkunit;

var
  a: system.jlstring;
begin
  a:='abc';
  jlsystem.fout.println(a);
end.
