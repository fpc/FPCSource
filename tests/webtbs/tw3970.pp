{ %OPT=-Sew }

{ Source provided for Free Pascal Bug Report 3970 }
{ Submitted by "Yiannis Dondos" on  2005-05-15 }
{ e-mail: dondos@otenet.gr }

{$warnings+}
program test1;

var
   f: Text;
   i: Integer;
   s: ShortString;

begin
     Assign (f, 'tw3970.tmp');
     Rewrite (f);
     writeln(f,'1');
     writeln(f,'2');
     close(f);

     Assign (f, 'tw3970.tmp');
     Reset (f);

     ReadLn (f, i);
     ReadLn (f, s); // Warning!

     Close (f);
     Erase (f);
end.
