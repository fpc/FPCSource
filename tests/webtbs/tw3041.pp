{ Source provided for Free Pascal Bug Report 3041 }
{ Submitted by "C Western" on  2004-04-06 }
{ e-mail: mftq75@dsl.pipex.com }
program bug2;

{$mode objfpc}{$H+}

uses
  Classes;
type
  TMyCollectionItem = class(TCollectionItem)
    public
      procedure Assign(Source: TPersistent); override;
  end;

procedure TMyCollectionItem.Assign(Source: TPersistent);
begin
end;

var
  A, B: TCollection;
  C: TMyCollectionItem;
begin
  A := TCollection.Create(TMyCollectionItem);
  B := TCollection.Create(TMyCollectionItem);
  C := TMyCollectionItem.Create(A);
  Writeln(A.Count);
  B.Assign(A);
  Writeln(B.Count);
  if B.Count<>A.Count then
    begin
      writeln('Error!');
      halt(1);
    end;
end.
