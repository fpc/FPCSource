{ Source provided for Free Pascal Bug Report 2270 }
{ Submitted by "marco" on  2002-12-19 }
{ e-mail: marco@freepascal.org }
type zheenum = (aa,bb,cc);
     zheset  = set of zheenum;

var l :word;
    o :zheenum;

begin
  if o IN zheset(lo(l)) Then
   ;
end.


