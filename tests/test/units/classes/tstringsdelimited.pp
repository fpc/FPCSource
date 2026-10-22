{ TStrings.CommaText and DelimitedText: output, and CommaText does not change Delimiter, QuoteChar or StrictDelimiter }
program tstringsdelimited;

{$mode objfpc}{$H+}

uses
  Classes;

type
  // String list that records the special characters seen while its strings are read.
  TWatchedStrings = class(TStringList)
  protected
    function Get(Index: Integer): string; override;
  public
    SeenDelimiter: Char;
    SeenQuoteChar: Char;
    SeenStrict: Boolean;
    // Exposes DoGetDelimitedText.
    function CallDoGetDelimitedText(aDelimiter, aQuoteChar: Char; aStrictDelimiter: Boolean): string;
  end;

var
  gErrors: Integer = 0;


function TWatchedStrings.Get(Index: Integer): string;

begin
  SeenDelimiter:=Delimiter;
  SeenQuoteChar:=QuoteChar;
  SeenStrict:=StrictDelimiter;
  Result:=inherited Get(Index);
end;


function TWatchedStrings.CallDoGetDelimitedText(aDelimiter, aQuoteChar: Char; aStrictDelimiter: Boolean): string;

begin
  Result:=DoGetDelimitedText(aDelimiter,aQuoteChar,aStrictDelimiter);
end;


procedure Check(const aName, aActual, aExpected: string);

begin
  if aActual=aExpected then
    Writeln(aName,': ',aActual,' OK')
  else
    begin
    Writeln(aName,': ',aActual,' expected ',aExpected,' ERROR');
    Inc(gErrors);
    end;
end;


procedure CheckTrue(const aName: string; aValue: Boolean);

begin
  if aValue then
    Writeln(aName,' OK')
  else
    begin
    Writeln(aName,' ERROR');
    Inc(gErrors);
    end;
end;


var
  lList: TWatchedStrings;

begin
  lList:=TWatchedStrings.Create;
  try
    lList.AddStrings(['a','b c','d,e','f"g','','h;i','j''k']);
    lList.Delimiter:=';';
    lList.QuoteChar:='''';
    lList.StrictDelimiter:=True;

    Check('CommaText', lList.CommaText, 'a,"b c","d,e","f""g",,h;i,j''k');
    CheckTrue('CommaText keeps Delimiter', lList.Delimiter=';');
    CheckTrue('CommaText keeps QuoteChar', lList.QuoteChar='''');
    CheckTrue('CommaText keeps StrictDelimiter', lList.StrictDelimiter);
    CheckTrue('Strings are read with the list''s own settings during CommaText',
      (lList.SeenDelimiter=';') and (lList.SeenQuoteChar='''') and lList.SeenStrict);

    Check('DelimitedText strict', lList.DelimitedText, 'a;b c;d,e;f"g;;''h;i'';''j''''k''');
    lList.StrictDelimiter:=False;
    Check('DelimitedText not strict', lList.DelimitedText, 'a;''b c'';d,e;f"g;;''h;i'';''j''''k''');

    Check('DoGetDelimitedText comma', lList.CallDoGetDelimitedText(',','"',False), lList.CommaText);
    Check('DoGetDelimitedText pipe', lList.CallDoGetDelimitedText('|','"',True), 'a|b c|d,e|"f""g"||h;i|j''k');
    Check('DoGetDelimitedText without quote char', lList.CallDoGetDelimitedText(';',#0,False), 'a;b c;d,e;f"g;;h;i;j''k');
    CheckTrue('DoGetDelimitedText keeps Delimiter', lList.Delimiter=';');

    lList.AlwaysQuote:=True;
    Check('CommaText AlwaysQuote', lList.CommaText, '"a","b c","d,e","f""g","","h;i","j''k"');
    Check('DelimitedText AlwaysQuote', lList.DelimitedText, '''a'';''b c'';''d,e'';''f"g'';'''';''h;i'';''j''''k''');
    lList.AlwaysQuote:=False;

    lList.Clear;
    lList.Add(#9'tab');
    Check('CommaText control character', lList.CommaText, '"'#9'tab"');
    lList.StrictDelimiter:=True;
    Check('DelimitedText strict control character', lList.DelimitedText, #9'tab');

    lList.Clear;
    lList.Add('');
    Check('CommaText single empty string', lList.CommaText, '""');
    Check('DelimitedText single empty string', lList.DelimitedText, '''''');

    lList.Clear;
    Check('CommaText empty list', lList.CommaText, '');
    Check('DelimitedText empty list', lList.DelimitedText, '');
  finally
    lList.Free;
  end;
  Writeln(gErrors,' errors');
  Halt(gErrors);
end.
