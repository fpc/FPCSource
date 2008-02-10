unit SpellCheck;

{ Simple unit to simplify/OOP-ize pascal-style the aspell interface. Currently
  very limited, will be expanded eventually. Use like you wish. }

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Aspell;

type
  TSuggestionArray = array of string;
  
  { TSpellCheck }

  TSpellCheck = class
   protected
    FSpeller: PAspellSpeller;
    FMode: string;
    FEncoding: string;
    FLanguage: string;
    procedure SetEncoding(const AValue: string);
    procedure SetLanguage(const AValue: string);
    procedure SetMode(const AValue: string);
    procedure CreateSpeller;
    procedure FreeSpeller;
   public
    constructor Create;
    destructor Destroy; override;
    function SpellCheck(const Word: string): TSuggestionArray;
   public
    property Mode: string read FMode write SetMode;
    property Encoding: string read FEncoding write SetEncoding;
    property Language: string read FLanguage write SetLanguage;
  end;

implementation

const
  DEFAULT_ENCODING = 'utf-8';
  DEFAULT_LANGUAGE = 'en';
  DEFAULT_MODE     = '';

function GetDefaultLanguage: string;
begin
  Result := GetEnvironmentVariable('LANG');
  if Length(Result) = 0 then
    Result := DEFAULT_LANGUAGE;
end;

{ TSpellCheck }

procedure TSpellCheck.SetEncoding(const AValue: string);
begin
  FEncoding := aValue;
  CreateSpeller;
end;

procedure TSpellCheck.SetLanguage(const AValue: string);
begin
  FLanguage := aValue;
  CreateSpeller;
end;

procedure TSpellCheck.SetMode(const AValue: string);
begin
  FMode := aValue;
  CreateSpeller;
end;

procedure TSpellCheck.CreateSpeller;
var
  Config: Paspellconfig;
  Error: Paspellcanhaveerror;
begin
  Config := new_aspell_config();

  if Length(FLanguage) > 0 then
    aspell_config_replace(Config, 'lang', pChar(FLanguage));
  if Length(FEncoding) > 0 then
    aspell_config_replace(Config, 'encoding', pChar(FEncoding));
  if Length(FMode) > 0 then
    aspell_config_replace(Config, 'mode', pChar(FMode));

  Error := new_aspell_speller(Config);

  delete_aspell_config(Config);
  FreeSpeller;

  if aspell_error_number(Error) <> 0 then
    raise Exception.Create('Error on speller creation: ' + aspell_error_message(Error))
  else
    FSpeller := to_aspell_speller(Error);
end;

procedure TSpellCheck.FreeSpeller;
begin
  if Assigned(FSpeller) then begin
    delete_aspell_speller(FSpeller);
    FSpeller := nil;
  end;
end;

constructor TSpellCheck.Create;
begin
  FEncoding := DEFAULT_ENCODING;
  FLanguage := GetDefaultLanguage;
  FMode := DEFAULT_MODE;

  CreateSpeller;
end;

destructor TSpellCheck.Destroy;
begin
  FreeSpeller;
end;

function TSpellCheck.SpellCheck(const Word: string): TSuggestionArray;
var
  sgs: Paspellwordlist;
  elm: Paspellstringenumeration;
  tmp: pChar;
  i: Integer = 0;
begin
  SetLength(Result, 0);

  if aspell_speller_check(FSpeller, pChar(Word), Length(Word)) = 0 then begin
    sgs := aspell_speller_suggest(FSpeller, pChar(Word), Length(Word));
    elm := aspell_word_list_elements(sgs);

    repeat
      if i >= Length(Result) then
        SetLength(Result, Length(Result) + 10);

      tmp := aspell_string_enumeration_next(elm);

      if tmp <> nil then begin
        Result[i] := tmp;
        Inc(i);
      end;
    until tmp = nil;

    SetLength(Result, i);

    delete_aspell_string_enumeration(elm);
  end;
end;

end.

