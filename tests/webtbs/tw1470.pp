{$mode objfpc}

function bbb (a1, a2: DWord): Boolean;
begin
        bbb := False;
end;

procedure a;
var
        i, j, k: DWord;

begin
        try
                i := 0;
                j := 1;

                while (True) do
                        begin
                                k := 2;

                                try
                                        if (not bbb (i, j)) then
                                                break; {this break generates an invalid label}

                                        if (k = 2) then
                                                k := 3;

                                finally
                                        k := 2;
                                end;
                        end;

                for i := 1 to 20 do
                        j := j + 1;

                WriteLn (j);

        finally
                i := 0;
                j := 1;
        end;
end;

begin
        a;
end.
