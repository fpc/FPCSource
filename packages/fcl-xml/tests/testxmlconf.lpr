program testxmlconf;

uses xmlconf;

begin
  With TXMLConfig.Create(Nil) do
  try
    FileName:='test.xml';
    OpenKey('General');
    SetValue('one',1);
    SetValue('two',2);
    SetValue('extra/name','michael');
    Flush;
  finally
    Free;
  end;
  With TXMLConfig.Create(Nil) do
  try
    FileName:='test.xml';
    OpenKey('General');
    If GetValue('one',0)<>1 then
      Writeln('One does not match');
    If GetValue('two',0)<>2 then
      Writeln('Two does not match');
    if GetValue('extra/name','')<>'michael' then
      Writeln('Name does not match');
  finally
    Free;
  end;
end.

