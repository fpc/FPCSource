
type SomeType = ( SomeElem );

const ElemSet = [ SomeElem ];

var
  b  : boolean;
begin
   b:=(SomeElem in ElemSet);
   writeln(b);
   b:=(SomeElem in (ElemSet + []));
   writeln(b);
   if not b then
     halt(1);
end.
