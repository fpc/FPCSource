{ Source provided for Free Pascal Bug Report 3474 }
{ Submitted by "Alexey Barkovoy" on  2004-12-26 }
{ e-mail: clootie@ixbt.com }
unit uw3474b;

interface

{$INLINE ON}
{$MODE DELPHI}
function V_Failed(Status: HRESULT): Boolean; inline;

implementation

uses uw3474a;

function V_Failed(Status: HRESULT): Boolean;
begin
  Result := Status <> 0;
end;

end.
