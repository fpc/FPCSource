{ This unit tests the basic routines         }
{ which are usually coded in assembler       }
{ Mainly used in porting to other processors }
{********************************************}
{ Tested against Delphi 6 and Delphi 3       }
{********************************************}
program tmem;

const
  MAX_TABLE = 69;  { this value should not be even ! }
  DEFAULT_VALUE = $55;
  FILL_VALUE = $33;


var
  dst_arraybyte : array[1..MAX_TABLE] of byte;
  src_arraybyte : array[1..MAX_TABLE] of byte;
  dst_arrayword : array[1..MAX_TABLE] of word;
  src_arrayword : array[1..MAX_TABLE] of word;
  dst_arraylongword : array[1..MAX_TABLE] of longword;
  src_arratlongword : array[1..MAX_TABLE] of longword;
  i: integer;


procedure test(value, required: longint);
begin
  if value <> required then
    begin
      writeln('Got ',value,' instead of ',required);
      halt(1);
    end;
end;




procedure test_fillchar;
 var
  i: integer;
 begin
  { non-aligned count }
  write('testing fillchar (non-aligned size)...');
  for i := 1 to MAX_TABLE do
    dst_arraybyte[i] := DEFAULT_VALUE;
  fillchar(dst_arraybyte, MAX_TABLE-2, FILL_VALUE);
  test(dst_arraybyte[MAX_TABLE], DEFAULT_VALUE);
  test(dst_arraybyte[MAX_TABLE-1], DEFAULT_VALUE);
  for i := 1 to MAX_TABLE-2 do
    test(dst_arraybyte[i], FILL_VALUE);
  writeln('Passed!');
  { modulo 2 count fill }
  write('testing fillchar (aligned size)...');
  for i := 1 to MAX_TABLE do
    dst_arraybyte[i] := DEFAULT_VALUE;
  fillchar(dst_arraybyte, MAX_TABLE-1, FILL_VALUE);
  test(dst_arraybyte[MAX_TABLE], DEFAULT_VALUE);
  for i := 1 to MAX_TABLE-1 do
    test(dst_arraybyte[i], FILL_VALUE);
  writeln('Passed!');
  { test zero fillchar count }
  write('testing fillchar (zero count)...');
  for i := 1 to MAX_TABLE do
    dst_arraybyte[i] := DEFAULT_VALUE;
  fillchar(dst_arraybyte, 0, FILL_VALUE);
  for i := 1 to MAX_TABLE do
    test(dst_arraybyte[i], DEFAULT_VALUE);
  writeln('Passed!');
  { test negative fillchar count }
  write('testing fillchar (negative count)...');
  for i := 1 to MAX_TABLE do
    dst_arraybyte[i] := DEFAULT_VALUE;
  fillchar(dst_arraybyte, -1, FILL_VALUE);
  writeln('Passed!');
 end;


procedure test_move;
begin
  { non-aligned count }
  write('testing move (non-aligned size)...');
  for i := 1 to MAX_TABLE do
  begin
    dst_arraybyte[i] := DEFAULT_VALUE;
    src_arraybyte[i] := FILL_VALUE;
  end;
  move(src_arraybyte, dst_arraybyte, MAX_TABLE-2);
  test(dst_arraybyte[MAX_TABLE], DEFAULT_VALUE);
  test(dst_arraybyte[MAX_TABLE-1], DEFAULT_VALUE);
  for i:= 1 to MAX_TABLE-2 do
    test(dst_arraybyte[i], FILL_VALUE);
  writeln('Passed!');
  { modulo 2 count fill }
  { non-aligned count }
  write('testing move (aligned size)...');
  for i := 1 to MAX_TABLE do
  begin
    dst_arraybyte[i] := DEFAULT_VALUE;
    src_arraybyte[i] := FILL_VALUE;
  end;
  move(src_arraybyte, dst_arraybyte, MAX_TABLE-1);
  test(dst_arraybyte[MAX_TABLE], DEFAULT_VALUE);
  for i:= 1 to MAX_TABLE-1 do
    test(dst_arraybyte[i], FILL_VALUE);
  writeln('Passed!');
  { zero move count }
  write('test move (zero count)...');
  for i := 1 to MAX_TABLE do
  begin
    dst_arraybyte[i] := DEFAULT_VALUE;
    src_arraybyte[i] := FILL_VALUE;
  end;
  move(src_arraybyte,dst_arraybyte, 0);
  for i:= 1 to MAX_TABLE do
    test(dst_arraybyte[i], DEFAULT_VALUE);
  writeln('Passed!');
  { negative move count }
  write('test move (negative count)...');
  move(src_arraybyte,dst_arraybyte,-12);
  writeln('Passed!');
end;

{$ifdef fpc}
procedure test_fillword;
 var
  i: integer;
 begin
  { non-aligned count }
  write('testing fillword (non-aligned size)...');
  for i := 1 to MAX_TABLE do
    dst_arrayword[i] := DEFAULT_VALUE;
  fillword(dst_arrayword, MAX_TABLE-2, FILL_VALUE);
  test(dst_arrayword[MAX_TABLE], DEFAULT_VALUE);
  test(dst_arrayword[MAX_TABLE-1], DEFAULT_VALUE);
  for i := 1 to MAX_TABLE-2 do
    test(dst_arrayword[i], FILL_VALUE);
  writeln('Passed!');
  { modulo 2 count fill }
  write('testing fillword (aligned size)...');
  for i := 1 to MAX_TABLE do
    dst_arrayword[i] := DEFAULT_VALUE;
  fillword(dst_arrayword, MAX_TABLE-1, FILL_VALUE);
  test(dst_arrayword[MAX_TABLE], DEFAULT_VALUE);
  for i := 1 to MAX_TABLE-1 do
    test(dst_arrayword[i], FILL_VALUE);
  writeln('Passed!');
  { test zero fillword count }
  write('testing fillword (zero count)...');
  for i := 1 to MAX_TABLE do
    dst_arrayword[i] := DEFAULT_VALUE;
  fillword(dst_arrayword, 0, FILL_VALUE);
  for i := 1 to MAX_TABLE do
    test(dst_arrayword[i], DEFAULT_VALUE);
  writeln('Passed!');
  { test negative fillword count }
  write('testing fillword (negative count)...');
  for i := 1 to MAX_TABLE do
    dst_arrayword[i] := DEFAULT_VALUE;
  fillword(dst_arrayword, -1, FILL_VALUE);
  writeln('Passed!');
 end;


procedure test_filldword;
 var
  i: integer;
 begin
  { non-aligned count }
  write('testing filldword (non-aligned size)...');
  for i := 1 to MAX_TABLE do
    dst_arraylongword[i] := DEFAULT_VALUE;
  filldword(dst_arraylongword, MAX_TABLE-2, FILL_VALUE);
  test(dst_arraylongword[MAX_TABLE], DEFAULT_VALUE);
  test(dst_arraylongword[MAX_TABLE-1], DEFAULT_VALUE);
  for i := 1 to MAX_TABLE-2 do
    test(dst_arraylongword[i], FILL_VALUE);
  writeln('Passed!');
  { modulo 2 count fill }
  write('testing filldword (aligned size)...');
  for i := 1 to MAX_TABLE do
    dst_arraylongword[i] := DEFAULT_VALUE;
  filldword(dst_arraylongword, MAX_TABLE-1, FILL_VALUE);
  test(dst_arraylongword[MAX_TABLE], DEFAULT_VALUE);
  for i := 1 to MAX_TABLE-1 do
    test(dst_arraylongword[i], FILL_VALUE);
  writeln('Passed!');
  { test zero filldword count }
  write('testing filldword (zero count)...');
  for i := 1 to MAX_TABLE do
    dst_arraylongword[i] := DEFAULT_VALUE;
  filldword(dst_arraylongword, 0, FILL_VALUE);
  for i := 1 to MAX_TABLE do
    test(dst_arraylongword[i], DEFAULT_VALUE);
  writeln('Passed!');
  { test negative filldword count }
  write('testing filldword (negative count)...');
  for i := 1 to MAX_TABLE do
    dst_arraylongword[i] := DEFAULT_VALUE;
  filldword(dst_arraylongword, -1, FILL_VALUE);
  writeln('Passed!');
 end;


procedure test_movechar0;
begin
  { non-aligned count }
  write('testing movechar0 (non-aligned size)...');
  for i := 1 to MAX_TABLE do
  begin
    dst_arraybyte[i] := DEFAULT_VALUE;
    src_arraybyte[i] := FILL_VALUE;
  end;
  movechar0(src_arraybyte, dst_arraybyte, MAX_TABLE-2);
  test(dst_arraybyte[MAX_TABLE], DEFAULT_VALUE);
  test(dst_arraybyte[MAX_TABLE-1], DEFAULT_VALUE);
  for i:= 1 to MAX_TABLE-2 do
    test(dst_arraybyte[i], FILL_VALUE);
  writeln('Passed!');
  { modulo 2 count fill }
  { non-aligned count }
  write('testing movechar0 (aligned size)...');
  for i := 1 to MAX_TABLE do
  begin
    dst_arraybyte[i] := DEFAULT_VALUE;
    src_arraybyte[i] := FILL_VALUE;
  end;
  movechar0(src_arraybyte, dst_arraybyte, MAX_TABLE-1);
  test(dst_arraybyte[MAX_TABLE], DEFAULT_VALUE);
  for i:= 1 to MAX_TABLE-1 do
    test(dst_arraybyte[i], FILL_VALUE);
  writeln('Passed!');
  { zero movechar0 count }
  write('test movechar0 (zero count)...');
  for i := 1 to MAX_TABLE do
  begin
    dst_arraybyte[i] := DEFAULT_VALUE;
    src_arraybyte[i] := FILL_VALUE;
  end;
  movechar0(src_arraybyte,dst_arraybyte, 0);
  for i:= 1 to MAX_TABLE do
    test(dst_arraybyte[i], DEFAULT_VALUE);
  writeln('Passed!');
  { withh null value as first value in index }
  write('test movechar0 with null character...');
  for i := 1 to MAX_TABLE do
  begin
    dst_arraybyte[i] := DEFAULT_VALUE;
    src_arraybyte[i] := 0;
  end;
  movechar0(src_arraybyte, dst_arraybyte, MAX_TABLE-1);
  { nothing should have been moved }
  for i:= 1 to MAX_TABLE do
    test(dst_arraybyte[i], DEFAULT_VALUE);
  writeln('Passed!');
  { with null value as second value in index }
  write('test movechar0 with null character (and char)...');
  for i := 1 to MAX_TABLE do
  begin
    dst_arraybyte[i] := DEFAULT_VALUE;
  end;
  src_arraybyte[1] := FILL_VALUE;
  src_arraybyte[2] := 0;
  movechar0(src_arraybyte, dst_arraybyte, MAX_TABLE-1);
  test(dst_arraybyte[1], FILL_VALUE);
  { the rest should normally not have bene touched }
  test(dst_arraybyte[2], DEFAULT_VALUE);
  writeln('Passed!');
end;
{$endif}


begin
  test_fillchar;
  test_move;
{$ifdef fpc}
  test_fillword;
  test_filldword;
  test_movechar0;
{$endif}
end.
