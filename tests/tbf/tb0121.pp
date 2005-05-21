{ %fail }

{$mode objfpc}

type
   TFPMemberVisibility = (
     visAutomated := 'A',
     visPublished := 'B',
     visPrivate := 'I',
     visProtected := 'O',
     visPublic := 'U',
     visDefault := 'd');

  TFPArray = array[TFPMemberVisibility] of String;

begin
end.
