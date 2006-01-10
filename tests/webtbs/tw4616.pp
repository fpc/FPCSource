{ Source provided for Free Pascal Bug Report 4616 }
{ Submitted by "Simon Felix" on  2005-12-18 }
{ e-mail: de@royalinc.ath.cx }
var
  t:array[0..128] of widechar;
begin
  //works as expected
  //t:='ab';

  //works as expected
  //t:=char(65)+'ab';

  //doesn't result in 'Aab' (result is 'A',#4190)
  //t:=widechar(65)+'ab';

  //this crashes the compiler
  t:=widechar(65)+'abc';
end.