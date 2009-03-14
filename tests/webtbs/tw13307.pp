program stf2;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

uses SysUtils;


var S: String;
    E: Extended;
    B: Boolean;
    Code: Integer;
begin
  DecimalSeparator := '.';
  ThousandSeparator := ',';
  writeln('DecimalSeparator  = ',DecimalSeparator);
  writeln('ThousandSeparator = ',ThousandSeparator);
  E := -1.0;
  S := '123.456';
  B := TextToFloat(PChar(S), E, fvExtended, DefaultFormatSettings);
  if B then writeln(Format('(1) "%s" -> %.3f',[S,E]))
  else
    begin
      writeln(Format('(1) "%s" -> Conversion Error',[S]));
      halt(1);
    end;
  B := TextToFloat(PChar(S), E, DefaultFormatSettings);
  if B then writeln(Format('(2) "%s" -> %.3f',[S,E]))
  else
    begin
      writeln(Format('(2) "%s" -> Conversion Error',[S]));
      halt(1);
    end;
  S := '123,456';
  B := TextToFloat(PChar(S), E, fvExtended, DefaultFormatSettings);
  if B then writeln(Format('(1) "%s" -> %.3f',[S,E]))
  else writeln(Format('(1) "%s" -> Conversion Error',[S]));
  B := TextToFloat(PChar(S), E, DefaultFormatSettings);
  if B then writeln(Format('(2) "%s" -> %.3f',[S,E]))
  else writeln(Format('(2) "%s" -> Conversion Error',[S]));



  DecimalSeparator := ',';
  ThousandSeparator := '.';
  writeln('DecimalSeparator  = ',DecimalSeparator);
  writeln('ThousandSeparator = ',ThousandSeparator);
  E := -1.0;
  S := '123.456';
  B := TextToFloat(PChar(S), E, fvExtended, DefaultFormatSettings);
  if B then
    begin
      writeln(Format('(1) "%s" -> %.3f',[S,E]));
      halt(1);
    end
  else writeln(Format('(1) "%s" -> Conversion Error',[S]));
  B := TextToFloat(PChar(S), E, DefaultFormatSettings);
  if B then
    begin
      writeln(Format('(2) "%s" -> %.3f',[S,E]));
      halt(1);
    end
  else writeln(Format('(2) "%s" -> Conversion Error',[S]));
  S := '123,456';
  B := TextToFloat(PChar(S), E, fvExtended, DefaultFormatSettings);
  if B then writeln(Format('(1) "%s" -> %.3f',[S,E]))
  else
    begin
      writeln(Format('(1) "%s" -> Conversion Error',[S]));
      halt(1);
    end;
  B := TextToFloat(PChar(S), E, DefaultFormatSettings);
  if B then writeln(Format('(2) "%s" -> %.3f',[S,E]))
  else
    begin
      writeln(Format('(2) "%s" -> Conversion Error',[S]));
      halt(1);
    end;

  DecimalSeparator := ',';
  ThousandSeparator := ',';
  writeln('DecimalSeparator  = ',DecimalSeparator);
  writeln('ThousandSeparator = ',ThousandSeparator);
  E := -1.0;
  S := '123.456';
  B := TextToFloat(PChar(S), E, fvExtended, DefaultFormatSettings);
  if B then
    begin
      writeln(Format('(1) "%s" -> %.3f',[S,E]));
      halt(1);
    end
  else writeln(Format('(1) "%s" -> Conversion Error',[S]));
  B := TextToFloat(PChar(S), E, DefaultFormatSettings);
  if B then
    begin
      writeln(Format('(12 "%s" -> %.3f',[S,E]));
      halt(1);
    end
  else writeln(Format('(2) "%s" -> Conversion Error',[S]));
  S := '123,456';
  B := TextToFloat(PChar(S), E, fvExtended, DefaultFormatSettings);
  if B then writeln(Format('(1) "%s" -> %.3f',[S,E]))
  else
    begin
      writeln(Format('(1) "%s" -> Conversion Error',[S]));
      halt(1);
    end;
  B := TextToFloat(PChar(S), E, DefaultFormatSettings);
  if B then writeln(Format('(2) "%s" -> %.3f',[S,E]))
  else
    begin
      writeln(Format('(2) "%s" -> Conversion Error',[S]));
      halt(1);
    end;

  DecimalSeparator := '.';
  ThousandSeparator := '.';
  writeln('DecimalSeparator  = ',DecimalSeparator);
  writeln('ThousandSeparator = ',ThousandSeparator);
  E := -1.0;
  S := '123.456';
  B := TextToFloat(PChar(S), E, fvExtended, DefaultFormatSettings);
  if B then writeln(Format('(1) "%s" -> %.3f',[S,E]))
  else
    begin
      writeln(Format('(1) "%s" -> Conversion Error',[S]));
      halt(1);
    end;
  B := TextToFloat(PChar(S), E, DefaultFormatSettings);
  if B then writeln(Format('(2) "%s" -> %.3f',[S,E]))
  else
    begin
      writeln(Format('(2) "%s" -> Conversion Error',[S]));
      halt(1);
    end;
  S := '123,456';
  B := TextToFloat(PChar(S), E, fvExtended, DefaultFormatSettings);
  if B then
    begin
      writeln(Format('(1) "%s" -> %.3f',[S,E]));
      halt(1);
    end
  else writeln(Format('(1) "%s" -> Conversion Error',[S]));
  B := TextToFloat(PChar(S), E, DefaultFormatSettings);
  if B then
    begin
      writeln(Format('(2) "%s" -> %.3f',[S,E]));
      halt(1);
    end
  else writeln(Format('(2) "%s" -> Conversion Error',[S]));


end.

