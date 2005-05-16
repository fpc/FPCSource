{
  $Id: invgenex.pas,v 1.2 2005/02/14 17:13:21 peter Exp $

}

program invgenex;
uses typ, iom, inv;
const n = 4;
var  term : arbint;
        A : array[1..n,1..n] of arbfloat;
begin
  assign(input, paramstr(1)); reset(input);
  assign(output, paramstr(2)); rewrite(output);
  writeln('program results invgenex');
  { Read matrix A}
  iomrem(input, A[1,1], n, n, n);
  { Print matrix A }
  writeln; writeln('A =');
  iomwrm(output, A[1,1], n, n, n, numdig);
  { Calculate inverse of A}
  invgen(n, n, A[1,1], term);
  writeln; writeln('term=', term:2);
  if term=1 then
  { Print inverse of matrix A}
  begin
      writeln; writeln('inverse of A =');
      iomwrm(output, A[1,1], n, n, n, numdig);
  end; {term=1}
  close(input); close(output)
end.

{

$Log: invgenex.pas,v $
Revision 1.2  2005/02/14 17:13:21  peter
  * truncate log

}
