program tw20836;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this };

{.$R *.res}
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
