Program Example69;

{ Program to demonstrate the FNMatch function. }

Uses oldlinux;

  Procedure TestMatch(Pattern,Name : String);

  begin
    Write ('"',Name,'" ');
    If FNMatch (Pattern,Name) then
      Write ('matches')
    else
      Write ('does not match');
    Writeln(' "',Pattern,'".');
  end;

begin
  TestMatch('*','FileName');
  TestMatch('.*','FileName');
  TestMatch('*a*','FileName');
  TestMatch('?ile*','FileName');
  TestMatch('?','FileName');
  TestMatch('.?','FileName');
  TestMatch('?a*','FileName');
  TestMatch('??*me?','FileName');
end.
