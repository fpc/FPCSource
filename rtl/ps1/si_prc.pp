{$SMARTLINK OFF}
{$GOTO ON}
unit si_prc;
interface

implementation

procedure PascalMain; external name 'PASCALMAIN';

var
  _bss_start: record end; external name '__bss_start__';
  _bss_end: record end; external name '__bss_end__';


function _FPC_proc_start: longint; cdecl; public name '_start';
label _loop, _exit;
begin
// fill the bss section with zeros
	asm

		la $t0, _bss_end
		la $t1, _bss_start
		sub $t0, $t0, $t1

	_loop:
		beq $t0, 0, _exit
		nop

		la $t1, _bss_start
		add $t1,$t1,$t0
		sw $0, 0($t1)

		addi $t0,$t0,-4
		j _loop
		nop
	_exit:

	end;

	PascalMain;

end; 

begin
end.