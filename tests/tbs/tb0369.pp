
type
  ptchar=^tchar;
  tchar=packed record
    c : char;
  end;

function inl(l:ptchar):ptchar;
begin
  inc(l);
  inl:=l;
end;

var
  i : longint;
  j : ptchar;
  s : string;
  error : boolean;
begin
  error:=false;
  s:='012345789';
  j:=@s[1];
  for i:=1to 8 do
   begin
     writeln(inl(j)^.c);
     If (inl(j)^.c<>s[i+1]) Then
      error:=true;
     inc(j);
   end;
  if error then
   begin
     writeln('Error!');
     halt(1);
   end;
end.
