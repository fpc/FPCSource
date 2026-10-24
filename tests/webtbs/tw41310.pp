{
  The test source tw41340.pp
  was first moved from webtbs to webtbf
  but this leads to troubles with
  the testsuite database.

  Thus a successful test
  is reintroduced in webtbs directory.

  Note: This test relies on
  the value of the constant fpc_in_lo_word
  from rtl/inc/innr.inc
}

program ie2017110102;

const
  fpc_in_lo_word = 1;

function lo_word(i : word) : byte; [internproc: fpc_in_lo_word];

var
  i : word;
  j : word;
  d : dword;
  q : qword;

begin
  i:=$1234;
  j:=lo_word(i);
  d:=lo_word(i);
  q:=lo_word(i);
  if (j<>$34) or (d<>$34) or (q<>$34) then
    begin
      writeln('Error in lo_word internal procedure');
      halt(1);
    end
  else
    writeln('Internal procedure lo_word works');
end.
