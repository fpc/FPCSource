{ %fail }

{ Source provided for Free Pascal Bug Report 3342 }
{ Submitted by "Alan Mead" on  2004-10-05 }
{ e-mail: cubrewer@yahoo.com }
{$ifdef fpc}{$MODE Delphi}{$endif}

unit tw3342;

Interface

uses
 Classes;

type
  TAgreementMatrix = class(TObject)
    private
      _Count: Array[1..100,1..100] of Integer;
    public
      constructor Create;
      destructor Destroy; override;
      property Count[Row,Column:Integer]:Integer read _Count[Row,Column] write _Count[Row,Column]; default;
  end;

Implementation

constructor TAgreementMatrix.Create;
begin
  inherited;
end;

destructor TAgreementMatrix.Destroy;
begin
  inherited;
end;

begin
end.
