{ %fail }

{ Source provided for Free Pascal Bug Report 3531 }
{ Submitted by "Christian Iversen" on  2005-01-07 }
{ e-mail: chrivers@iversen-net.dk }
Program Bug;

Type
  XEscapeResolve =
    Record
      C : Char;
      E : String;
    End;

Const
  XStandardFold1 : Array Of longint = (1);
  XStandardFold2 : Array Of XEscapeResolve =
  ((C: 'n'; E: #10));

Begin
End.
