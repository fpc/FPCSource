{ %FAIL }

unit tw24453;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TIterator }

  TIterator<T> = class
  end;

  TAncestor<T> = class
  public type
    TAncestorIterator = class(TIterator<T>)
    end;
  end;

  TTestClass<T> = class(TAncestor<T>)
  private
    // this compiler recognise
    fAncIterator: TAncestor<T>.TAncestorIterator;
  protected
    // this however does not compile, compiler error is
    // ugenericsnestedclassdeclaration.pas(29,39) Fatal: Syntax error, ";" expected but "." found
    // the same problem as with result type is  with arguments of methods aswell

    //function GetIterator: TAncestor<T>.TAncestorIterator;

    // this compile, but not compatible with delphi (at least with delphi XE2, which I am using)
    function GetIterator: TAncestorIterator<T>;
  end;

implementation

function TTestClass<T>.GetIterator: TAncestorIterator<T>;
begin
  Result := fAncIterator;
end;

end.

