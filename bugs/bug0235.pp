program bug0233;

var s:string;
    w:cardinal;
    code:word;

begin
    s:='192';
    val(s,w,code);
    if code<>0 then
        writeln('Error')
    else
        writeln(w);
end.
