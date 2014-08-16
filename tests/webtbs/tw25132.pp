program tw25132;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  uw25132;

type
  TAnotherIterator = class(TIterator<TObject>)
  public
    function    GetValue(): Integer; override;
  end;
  
  TCollection = class(TObject)
  private
    function    CreateAnotherIterator(): TIterator<TObject>; virtual;
    function    CreateIterator(): TIterator<TObject>; virtual;
  end;

{ TAnotherIterator }

function TAnotherIterator.GetValue(): Integer; 
begin
  Result := 2;
end;
  
{ TCollection}  
  
function TCollection.CreateAnotherIterator(): TIterator<TObject>;
begin
  Result := TAnotherIterator.Create();
end;

function TCollection.CreateIterator(): TIterator<TObject>;
begin
  Result := TCollectionIterator.Create();
end;

var
  CollectionIterator: TCollectionIterator;
  AnotherIterator:    TAnotherIterator;
begin
  CollectionIterator := TCollectionIterator.Create();
  AnotherIterator    := TAnotherIterator.Create();

  if CollectionIterator.GetValue() = 1 then
    WriteLn('Collection iterator: OK')
  else
  begin
    WriteLn('Collection iterator: FAILED');
    Halt(1);
  end;
  
  if AnotherIterator.GetValue() = 2 then
    WriteLn('Another iterator: OK')
  else
  begin
    WriteLn('Another iterator: FAILED');  
    Halt(1);
  end;
  
  CollectionIterator.Free();
  AnotherIterator.Free();
end.
