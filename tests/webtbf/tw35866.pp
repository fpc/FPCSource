{ %FAIL }

program tw35866;
{$mode delphi}{$warn 5079 off} // turn warning experimental off
uses
  sysutils, dateutils, typinfo, rtti, classes;

type
{$M+}
   TDateTimeAttribute = class(TCustomAttribute)
   private
     FArg:TDateTime;
   public
     constructor Create(aArg: String);overload;
     constructor Create(aArg: TDateTime);overload;
     constructor Create(aArg: int64);overload;
     property DateTime:TDateTime read Farg;
   end;

   //[TDateTimeAttribute]
   TMyDateTimeClass = class
   private
     FDateTime:TDateTime;
   published
     [TDateTimeAttribute(Now)]
     property DateTime:TDateTime read FDateTime;
   end;

   constructor TDateTimeAttribute.Create(aArg: String);
   begin
     inherited create;
     FArg := StrToDateTime(aArg);
   end;

   constructor TDateTimeAttribute.Create(aArg: TDateTime);
   begin
     FArg := aArg;
   end;

   constructor TDateTimeAttribute.Create(aArg: int64);
   begin
     //FArg := UnixToDateTime(aArg);
   end;


var
  Test:TMyDateTimeClass;
begin
  Test := TMyDateTimeClass.Create;
  try
    writeln(DateTimeToStr(Test.DateTime));
  finally
    test.free;
  end;
end.
