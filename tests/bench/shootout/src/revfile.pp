{ Reverse a File }

Program reversefile;
uses SysUtils, Classes;

var
    i, N : longint;
    list : TList;
    line : string;
    pline : pointer;
begin
    list := TList.Create;
    While Not Eof(input) do
    begin
        Readln(input, line);
        Getmem(pline, Length(line)+1);
        Move(line, pline^, Length(line)+1);
        list.Add( pline );
    end;
    N := list.Count;
    For i := N-1 Downto 0 do WriteLn( string(list.items[i]^) );
end.
