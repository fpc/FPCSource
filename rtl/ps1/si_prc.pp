unit si_prc;

interface

implementation

procedure PascalMain; external name 'PASCALMAIN';

{ this function must be the first in this unit which contains code }
function _FPC_proc_start: longint; cdecl; public name '_start';
begin
		PascalMain;
end; 

begin
end.