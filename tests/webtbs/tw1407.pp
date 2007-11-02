{ Source provided for Free Pascal Bug Report 1407 }
{ Submitted by "vigo von harrach" on  2001-02-13 }
{ e-mail: wingo@fh-konstanz.de }
{ compiled with 1.04 on win32 }
{ options : -B -CX -XXs -OG2p3 -So }
{$mode tp}

var
        a : array[1..10] of integer;
        i : byte;
begin
        { both give compile time errors }
        for a[1] := 1 to 10 do begin end;
        for a[10] := 1 to 10 do begin end;
        {i:=5;
        for a[i] := 1 to 10 do begin end;
        this fails in BP }
end.
