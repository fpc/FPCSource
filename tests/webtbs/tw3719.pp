{ Source provided for Free Pascal Bug Report 3719 }
{ Submitted by "Markus Beth" on  2005-03-01 }
{ e-mail: markus.beth@zkrd.de }

{$ifdef fpc}{$mode delphi}{$endif}

uses
  Classes;

begin
  with TStringList.Create do try
    Delimiter := ' ';
    QuoteChar := #0;
    Add('1');
    Add('2');
    WriteLn(DelimitedText);
    if DelimitedText<>'1 2' then
      halt(1);
  finally
    Free;
  end;
end.
