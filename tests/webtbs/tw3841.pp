{ Source provided for Free Pascal Bug Report 3841 }
{ Submitted by "Joost v.d. Sluis" on  2005-03-28 }
{ e-mail:  }
{$mode objfpc}

type TMyClass = Class(TObject)
     private
       Function GetInte : Integer;
     public
       property Inte : Integer read GetInte;
     end;

var buf     : longint;
    MyClass : TMyClass;

function TMyClass.GetInte : integer;
begin
  Result := 1010;
end;

begin
  MyClass.Create;
  move(MyClass.inte,buf,sizeof(longint));
end.
