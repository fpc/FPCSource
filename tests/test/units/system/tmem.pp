{ This unit tests the basic routines         }
{ which are usually coded in assembler       }
{ Mainly used in porting to other processors }
program tmem;

const
  MAX_TABLE = 69;  { this value should not be even ! }
  DEFAULT_VALUE = $55;
  FILL_VALUE = $33;
  
  
var
  dst_array : array[1..MAX_TABLE] of byte;
  src_array : array[1..MAX_TABLE] of byte;
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
    dst_array[i] := DEFAULT_VALUE;
  fillchar(dst_array, MAX_TABLE-2, FILL_VALUE);
  test(dst_array[MAX_TABLE], DEFAULT_VALUE);
  test(dst_array[MAX_TABLE-1], DEFAULT_VALUE);
  for i := 1 to MAX_TABLE-2 do
    test(dst_array[i], FILL_VALUE);
  writeln('Passed!');
  { modulo 2 count fill }
  write('testing fillchar (aligned size)...');
  for i := 1 to MAX_TABLE do
    dst_array[i] := DEFAULT_VALUE;
  fillchar(dst_array, MAX_TABLE-1, FILL_VALUE);
  test(dst_array[MAX_TABLE], DEFAULT_VALUE);
  for i := 1 to MAX_TABLE-1 do
    test(dst_array[i], FILL_VALUE);
  writeln('Passed!');
 end;


procedure test_move;
begin
  { non-aligned count }
  write('testing move (non-aligned size)...');
  for i := 1 to MAX_TABLE do
  begin
    dst_array[i] := DEFAULT_VALUE;
    src_array[i] := FILL_VALUE;
  end;  
  move(src_array, dst_array, MAX_TABLE-2);
  test(dst_array[MAX_TABLE], DEFAULT_VALUE);
  test(dst_array[MAX_TABLE-1], DEFAULT_VALUE);
  for i:= 1 to MAX_TABLE-2 do
    test(dst_array[i], FILL_VALUE);
  writeln('Passed!');
  { modulo 2 count fill }
  { non-aligned count }
  write('testing move (aligned size)...');
  for i := 1 to MAX_TABLE do
  begin
    dst_array[i] := DEFAULT_VALUE;
    src_array[i] := FILL_VALUE;
  end;  
  move(src_array, dst_array, MAX_TABLE-1);
  test(dst_array[MAX_TABLE], DEFAULT_VALUE);
  for i:= 1 to MAX_TABLE-1 do
    test(dst_array[i], FILL_VALUE);
  writeln('Passed!');
end;



begin
  test_fillchar;
  test_move;
end.

{
  $Log$
  Revision 1.1  2002-03-05 21:52:00  carl
  basic mem testing

}