{$mode delphi}

uses
  Variants
  ;

type


  // TMockMethod
  //
  TMockMethod = class
  private 
    FReturnValue: variant; 
    
  public

    //: Set return value
    procedure Returns(AValue: Variant); overload;
    procedure Returns(AValue: Pointer); overload; // if i change this from type Pointer to Double it works
    procedure Returns(AValue: Integer); overload;
  end;


function Failure: TMockMethod;
begin
  Result := TMockMethod.Create;

  { TODO: Free Pascal Compiler version 2.2.0 [2007/08/30] for i386 crash with Internal error 2006122804 on this line
	using fpc -Sd PascalMockBug.pas or fpc -S2 PascalMockBug.pas
  }
  Result.Returns(Result.FReturnValue);
end;


{ TMockMethod }


procedure TMockMethod.Returns(AValue: Integer);
begin
  halt(1);
end;

procedure TMockMethod.Returns(AValue: Pointer);
begin
  halt(1);
end;

procedure TMockMethod.Returns(AValue: Variant);
begin
  writeln('ok');
end;

var
  c: tmockmethod;
begin
  c:=Failure;
  c.free;
end.

