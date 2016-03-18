var
  aURI,Server:rawbytestring;

begin
  aURI:='abcdefg';
  SetString(Server,@aURI[1],Length(aURI));
  if stringcodepage(server)=CP_NONE then
    halt(1);
end.
