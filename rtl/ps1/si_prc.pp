{$SMARTLINK OFF}
unit si_prc;
interface

implementation

procedure PascalMain; external name 'PASCALMAIN';

var
  _bss_start: record end; external name '__bss_start__';
  _bss_end  : record end; external name '__bss_end__';

function _FPC_proc_start: LongInt; cdecl; public name '_start';
begin
  asm
    la    $t0, _bss_start
    la    $t1, _bss_end
  .Lbss_loop:
    beq   $t0, $t1, .Lbss_done
    nop
    sw    $zero, 0($t0)
    addiu $t0, $t0, 4
    b     .Lbss_loop
    nop
  .Lbss_done:
  end;

  PascalMain;

  { PascalMain should normally never return. }
  while True do
    ;
end;

begin
end.
