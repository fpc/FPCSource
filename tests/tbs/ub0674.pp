{$mode objfpc}{$H+}

unit ub0674;

interface

type
   TMyClass = class
   public
     generic function CreateObjectFromJSONString<T{: TObject}>(AJSONString: String; ADescriptionTag: string = ''): T;
   end;

implementation

generic function TMyClass.CreateObjectFromJSONString<T>(AJSONString: String; ADescriptionTag: string): T;
begin
  Result:=Nil;//T.Create;
end;

end.
