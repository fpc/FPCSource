{ original: peter5d.pas from the GNU Pascal testsuite }

{$mode macpas}
program peter5d(output);

        type
                obj = object
                        procedure Destroy;
                        procedure Free;
                end;

        procedure obj.Destroy;
        begin
                dispose( self );
        end;

        procedure obj.Free;
        begin
           writeln('must not be called');
           halt(1);
        end;

        var
                o: obj;
begin
        new(o);
        o.Destroy;
        WriteLn( 'OK' );
end.
