Program Example89;

{ This program demonstrates the FormatFloat function }

Uses sysutils;

Const
  NrFormat=9;
  FormatStrings : Array[1..NrFormat] of string = (
        '',
        '0',
        '0.00',
        '#.##',
        '#,##0.00',
        '#,##0.00;(#,##0.00)',
        '#,##0.00;;Zero',
        '0.000E+00',
        '#.###E-0');
  NrValue = 5;
  FormatValues : Array[1..NrValue] of Double =
    (1234,-1234,0.5,0,-0.5);

  Width  = 12;
  FWidth = 20;

Var
  I,J : Integer;
  S : String;

begin
  Write('Format':FWidth);
  For I:=1 to NrValue do
    Write(FormatValues[i]:Width:2);
  Writeln;
  For I:=1 to NrFormat do
    begin
    Write(FormatStrings[i]:FWidth);
    For J:=1 to NrValue do
      begin
      S:=FormatFloat(FormatStrings[I],FormatValues[j]);
      Write(S:Width);
      end;
    Writeln;
    end;
End.