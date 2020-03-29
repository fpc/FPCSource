program tb0634;

var
  s, s1, s2, s3, s4: String;
begin
  s := Concat('Hello', ' ', 'World');
  if s <> 'Hello World' then
    Halt(1);
  s := Concat('Hello');
  if s <> 'Hello' then
    Halt(2);
  s1 := 'Hello';
  s2 := 'Free';
  s3 := 'Pascal';
  s4 := 'World';
  s := Concat(s1, ' ', s2, ' ', s3, ' ', s4);
  if s <> 'Hello Free Pascal World' then
    Halt(3);
  s := Concat(Concat(s1, ' ', s2), ' ', Concat(s3, ' ', s4));
  if s <> 'Hello Free Pascal World' then
    Halt(4);
  Writeln('ok');
end.
