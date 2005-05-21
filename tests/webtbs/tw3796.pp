{ Source provided for Free Pascal Bug Report 3796 }
{ Submitted by "Antonio Marti" on  2005-03-15 }
{ e-mail: windowze2000@yahoo.es }
{
FAILS WITH: fpc -Mobjfpc test.pp
WHY? IS IT A BUG?
}
unit tw3796;

{$mode objfpc}

interface

uses objects;

type
  PType                 = ^TType;
  TType                 = object(TObject)
    private
      function IsEmpty: Boolean;
    public
      property Empty : Boolean Read IsEmpty;
  end;

implementation

function TType.IsEmpty: Boolean;
begin
  result:=True;
end;

end.
