{ %delfiles=EMPTY_TYPED_FILE}
{ %OPT=-Sr }
{$mode iso }
{ test that reset(file) sets eof if the file is empty
  and no runtime error occurs }
program test(output, empty_typed_file);
var
  empty_typed_file: file of integer;
begin
  rewrite(empty_typed_file);
  close(empty_typed_file);
  reset(empty_typed_file);
  if eof(empty_typed_file) then
    writeln('ok')
  else
    halt(1);
end.
