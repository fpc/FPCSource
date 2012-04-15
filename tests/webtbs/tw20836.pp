{ %NORUN }

{ adjusted test by removing some "Lazarusisms" }
program tw20836;

{$mode objfpc}{$H+}

type
  generic TGObjectChangeCommand<_T>=object
                                        private
                                        DoData,UnDoData:_T;
                                        method:tmethod;
                                        public
                                        procedure UnDo;virtual;
                                    end;
  TCommand=specialize TGObjectChangeCommand<Integer>;
procedure TGObjectChangeCommand.UnDo;
type
    TCangeMethod=procedure(const data:_T)of object;
begin
     TCangeMethod(method)(UnDoData);
end;
begin
end.
