{ Source provided for Free Pascal Bug Report 3063 }
{ Submitted by "Werner Bochtler" on  2004-04-23 }
{ e-mail: werner.bochtler@zkrd.de }
program scope;

{$ifdef fpc}{$mode delphi}{$endif}

uses Classes;

var
  error: string;
begin
  with TStringList.Create do try
    { TStringList.Error is protected and is not visible }
    error := 'some text';
    WriteLn( error );
  finally
    free;
  end;
end.
