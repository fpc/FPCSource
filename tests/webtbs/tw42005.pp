{ Regression test for x86 OptPass1MOV / TryConstMerge (compiler/x86/aoptx86.pas).

  Constant stores to the stack that are NOT adjacent must not be merged into
  one wider store, because that overwrites the bytes in between.
  TryConstMerge selected the merge width from the outer instruction (p)
  instead of the pair being merged (p1/p2).

  Run with -O- -O2 -O3 -O4.  Exit code 0 = OK, otherwise number of the failed test. }

{$mode objfpc}
{$R-}{$Q-}

var
  failed: longint = 0;

procedure check(const a: array of byte; const expect: array of byte; id: longint); noinline;
  var
    i: longint;
  begin
    for i := 0 to high(expect) do
      if a[i] <> expect[i] then
        begin
          writeln('FAIL ', id, ': byte ', i, ' is ', a[i], ', expected ', expect[i]);
          if failed = 0 then failed := id;
          exit;
        end;
  end;

{ byte stores at 0,1 (merged to a word), then at 4 and 6 (must not become a dword store) }
procedure t1; noinline;
  var
    r: array[0..7] of byte;
  begin
    r[5] := $55; r[7] := $77; r[2] := $22; r[3] := $33;
    r[0] := 1; r[1] := 2; r[4] := 3; r[6] := 4;
    check(r, [1, 2, $22, $33, 3, $55, 4, $77], 1);
  end;

{ word stores at 0,2 (merged to a dword), then at 8 and 12 (second one is 0, so the
  merged constant would still fit into 32 bits) }
procedure t2; noinline;
  var
    r: array[0..15] of byte;
  begin
    r[4] := 0; r[5] := 0; r[6] := 0; r[7] := 0;
    r[10] := $AA; r[11] := $BB; r[14] := $CC; r[15] := $DD;
    PWord(@r[0])^ := $0304; PWord(@r[2])^ := $0102;
    PWord(@r[8])^ := $1111; PWord(@r[12])^ := 0;
{$ifdef FPC_LITTLE_ENDIAN}
    check(r, [4, 3, 2, 1, 0, 0, 0, 0, $11, $11, $AA, $BB, 0, 0, $CC, $DD], 2);
{$else FPC_LITTLE_ENDIAN}
    check(r, [3, 4, 1, 2, 0, 0, 0, 0, $11, $11, $AA, $BB, 0, 0, $CC, $DD], 2);
{$endif FPC_LITTLE_ENDIAN}
  end;

{ the intended merges of directly adjacent stores must still give the right bytes }
procedure t3; noinline;
  var
    r: array[0..7] of byte;
  begin
    r[0] := 1; r[1] := 2; r[2] := 3; r[3] := 4; r[4] := 5; r[5] := 6; r[6] := 7; r[7] := 8;
    check(r, [1, 2, 3, 4, 5, 6, 7, 8], 3);
  end;

begin
  t1;
  t2;
  t3;
  if failed = 0 then writeln('OK');
  halt(failed);
end.
