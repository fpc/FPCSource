var
  s1, s2: ansistring;
begin
  s1 := 'abc';
  s2:='';
  { ensure the codepage of s1 is different from defaultsystemcodepage }
  if defaultsystemcodepage=866 then
    setcodepage(rawbytestring(s1),1251,false)
  else
    setcodepage(rawbytestring(s1),866,false); 
  { if the destination is empty, insert must create a new string
    with the same code page as the source }
  Insert(s1, s2, 1);
  if StringRefCount(s1)<>1 then
    halt(1);
  if StringRefCount(s2)<>1 then
    halt(2);
  if stringcodepage(s2)<>stringcodepage(s1) then
    halt(3);
end.
