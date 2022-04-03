{ %fail }

program T001;
{$MODE DELPHI}
{$R-}
type
  TLanguages = (
    lOne,
    lTwo,
    lThree,
    lFour
  );

  TLanguagesSub = lOne..lTwo;
 
const
  LANGUAGE_NONE = TLanguages(255);
 
type
 TLanguage = record
   Index : TLanguagesSub;
 end;
 
var
  Lang: TLanguages;
  CurrentLanguage: TLanguage = (
    Index:  LANGUAGE_NONE
  );
 
begin
  Lang := LANGUAGE_NONE;
end.
