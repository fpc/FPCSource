program generictest5;

{$mode delphi}{$H+}

type

   TRec<T> = record
      Value : T;
   end;
   TRecArray<T> = array of TRec<T>;

   { TFoo }

   TFoo<T> = class
     FArr : TRecArray<T>;
     procedure Test;
   end;

{ TFoo<T> }

procedure TFoo<T>.Test;
begin
  SetLength(FArr, 1);
end;

begin
end.
