{$mode objfpc}
uses
  sysutils;

resourcestring sMyNewErrorMessage = 'Illegal value: %d';

begin
  raise Exception.CreateResFmt(@sMyNewErrorMessage, [-1]);
end.
