program tmultilinestring23;

{$mode ObjFPC}{$H+}
{$modeswitch PrefixedAttributes}
{$modeswitch MultiLineStrings}
{$MultiLineStringTrimLeft Auto}

uses RTTI;

type
  TMultiLineAttribute = class(TCustomAttribute)
  private
    FString: String;
  public
    constructor Create(const S: String);
    property StringValue: String read FString;
  end;

  constructor TMultiLineAttribute.Create(const S: String);
  begin
    FString := S;
  end;

type
  [TMultiLineAttribute(
    `This is my
     pretty cool
     multi-line string
     attribute!`
  )]
  [TMultiLineAttribute(
    `This is my
     even cooler
     multi-line string
     attribute!`
  )]
  TMyClass = class
  end;

var
  A: TMultiLineAttribute;

begin
  with TRTTIType.Create(TypeInfo(TMyClass)) do
  begin
    for TCustomAttribute(A) in GetAttributes() do
      WriteLn(A.StringValue);
    Free();
  end;
end.
