var
  l : dword;
begin
  l:=$deadbeef;
  if SwapEndian(l)=l then
    halt(1);
  if SwapEndian(SwapEndian(l))<>l then
    halt(1);
  writeln('ok');
end.
