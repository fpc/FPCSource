{ Source provided for Free Pascal Bug Report 3621 }
{ Submitted by "Thomas Schatzl" on  2005-02-01 }
{ e-mail:  }
{$MODE OBJFPC}
type
        tfourcc=array[0..3] of char;

FUNCTION comp(f1, f2:tfourcc):boolean;
BEGIN
  comp:=((f1[0]=f2[0]) AND (f1[1]=f2[1]) AND (f1[2]=f2[2]) AND (f1[3]=f2[3]));
END;


begin
        comp('ABCD', 'DEFG');
end.
