{ %fail }

{ Source provided for Free Pascal Bug Report 4913 }
{ Submitted by "Vinzent Hoefler" on  2006-03-17 }
{ e-mail: ada.rocks@jlfencey.com }
const
   Some_String : String = '0123456789';

type
   Some_Enum = (Zero, One, Two, Three);

var
   i : Some_Enum;

begin
   WriteLn (Some_String[2]);   // Should fail if "Some_String = '...'";
   WriteLn (Some_String[Two]); // Should fail with type error.

   i := Three;
   WriteLn (Some_String[i]);
end.

