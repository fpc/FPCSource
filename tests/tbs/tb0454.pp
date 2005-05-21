program tb0454;
{ reported on fpc-devel by Jesus Reyes <jesusrmx@yahoo.com.mx> on 14 July 2003 }
{ as failing with 1.1                                                          }
{$mode objfpc}
var
  a,b: integer;
  c,d,e,f: boolean;

function Ok: boolean;
begin
        result := ( a = b )
                and c = d
                and e = f;
end;

var
        r: boolean;
begin
        a := 1;
        b := 2;
        c := false;
        d := true;
        e := false;
        f := true;

        r := Ok;
        if not r then
          begin
            writeln('error, result should be true');
            halt(1);
          end;
end.
