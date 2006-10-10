program test_mix_string_types;
{$define fail1}
{$define fail2}

{$ifdef fail1}
{$H+}
{$endif}

uses sysutils
	{$ifdef fail2}
	,classes
	{$endif}
	;
{$ifndef fail2}
type TComponentName=string;
{$endif}
var
  S1: TComponentName;
  S2:string;
begin
	if AnsiUpperCase(S1+'.'+S2)='' then ;
end.
