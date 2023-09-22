{ %FAIL }

{$mode objfpc}
unit tw40332;

interface

	function Bsr(const value: byte): byte; internproc: fpc_in_bsr_x;
	function Bsr(const value: word): cardinal; internproc: fpc_in_bsr_x;
	function Bsr(const value: dword): cardinal; internproc: fpc_in_bsr_x;
{$ifdef cpu64}
	function Bsr(const value: qword): cardinal; internproc: fpc_in_bsr_x;
{$endif}

type
	SomeEnum = (A, B, C, D);

const
	SomeEnumBits = 1 + Bsr(ord(High(SomeEnum)) or 1);

implementation

end.
