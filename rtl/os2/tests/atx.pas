program atx;

var f:text;
    s:string;

begin
    assign(f,'c:\autoexec.bat');
    reset(f);
    while not eof(f) do
        begin
            readln(f,s);
            writeln(s);
        end;
    close(f);
end.
