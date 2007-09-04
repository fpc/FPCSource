unit sCheck;

{$mode objfpc}{$H+}

interface

uses
  Aspell;
  
type
  TSuggestionArray = array of string;
  
  function SpellCheck(const Word, Lang: string; out Suggestions: TSuggestionArray): Integer;

var
  Encoding: string = 'utf-8';

implementation

function SpellCheck(const Word, Lang: string; out Suggestions: TSuggestionArray): Integer;
var
  cnf: aspellconfig;
  ape: aspellcanhaveerror;
  spl: aspellspeller;
  sgs: aspellwordlist;
  elm: aspellstringenumeration;
  tmp: pChar;
  i: Integer = 0;
begin
  SetLength(Suggestions, 10);
  Result := -1;

  cnf := new_aspell_config();

  aspell_config_replace(cnf, 'lang', pChar(Lang));
  aspell_config_replace(cnf, 'encoding', pChar(Encoding));

  ape := new_aspell_speller(cnf);

  delete_aspell_config(cnf);
  spl := nil;

  if aspell_error_number(ape) <> 0 then
    Exit
  else
    spl := to_aspell_speller(ape);

  if aspell_speller_check(spl, pChar(Word), Length(Word)) > 0 then
    Exit(0)
  else begin
    sgs := aspell_speller_suggest(spl, pChar(Word), Length(Word));
    elm := aspell_word_list_elements(sgs);

    repeat
      if i >= Length(Suggestions) then
        SetLength(Suggestions, Length(Suggestions) + 10);

      tmp := aspell_string_enumeration_next(elm);

      if tmp <> nil then begin
        Suggestions[i] := tmp;
        Inc(i);
      end;
    until tmp = nil;

    SetLength(Suggestions, i);

    Result := i;

    delete_aspell_string_enumeration(elm);
  end;

  delete_aspell_speller(spl);
end;

end.

