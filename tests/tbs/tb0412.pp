{ %opt=-Oonoconstprop }
{$mode objfpc}
{$r+}
uses
   sysutils;

var
   a : array of longint;
   b : longint;

begin
   try
     a[10]:=1;
   except
     setlength(a,3);
     a[0]:=1;
     a[1]:=1;
     a[2]:=1;
     try
       a[3]:=1;
     except
       try
         b:=-1;
         a[b]:=1;
       except
         halt(0);
       end;
     end;
   end;
   writeln('Problem with dyn. array range checking');
   halt(1);
end.
