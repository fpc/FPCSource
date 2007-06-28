{%NORUN}
{%FAIL}
program tb0200;

{$H-}

{TP rejects this code both with range checking off and on. However,
 we allow indexing arrays out of bounds with range checks off, so
 we best reject this then only with range checking on.}

{$Q+,R+}

var a:string;
    c:char;

begin
  a:='';
  c:=a[257];
end.