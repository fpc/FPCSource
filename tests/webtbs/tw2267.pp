{ Source provided for Free Pascal Bug Report 2267 }
{ Submitted by "Marco" on  2002-12-19 }
{ e-mail: marco@freepascal.org }
type theenum=(aa,bb,cc);

var i,j : integer;
    k  :theenum;

begin
 k:=theenum(i<j);
end.
