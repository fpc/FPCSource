{ %OPT=-g }
{ the debug info created problems for very long mangled names
  because the manglednames where shorten differently (PM)
  fixed in v 0.99.9 }
program ts010021;

var i : longint;

   type very_very_very_long_integer = longint;

  function ugly(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p :
                very_very_very_long_integer) : longint;

    begin
       ugly:=0;
    end;

begin
end.
