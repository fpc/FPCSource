{ Source provided for Free Pascal Bug Report 3063 }
{ Submitted by "Werner Bochtler" on  2004-04-23 }
{ e-mail: werner.bochtler@zkrd.de }
program scope;
uses classes;

{$mode delphi}

var
  error: string;
begin
  with TStringList.Create do try
    error := 'some text';
    WriteLn( error );
  finally
    free;
  end;
end.
