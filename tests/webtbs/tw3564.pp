{ Source provided for Free Pascal Bug Report 3564 }
{ Submitted by "Patrick Dietrich" on  2005-01-17 }
{ e-mail: patrick.dietrich@SPAM.informatik.ME.uni-ulm.NOT.de }

{$mode delphi}

type
   StringArray = array of string;

   TestClass = class(TObject)
   public
      FArr : StringArray;
      function getArr: StringArray;
      function getCopy: StringArray;
      constructor create;
      property arr : StringArray read getArr;
   end;

function TestClass.getArr: StringArray;
begin
   result := self.FArr;
end;

function TestClass.getCopy: StringArray;
begin
   Result := Copy(arr, 0, Length(arr)-1);
end; { getCopy }

constructor TestClass.create;
begin
   setLength( Farr, 3);

   Farr[0] := 'one';
   Farr[1] := 'two';
   Farr[2] := 'three';
end;

begin
end.
