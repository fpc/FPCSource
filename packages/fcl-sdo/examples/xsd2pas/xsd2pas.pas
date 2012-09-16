program xsd2pas;

{$mode objfpc}{$H+}

uses
  cwstring, Classes, SysUtils, sdo, sdo_datafactory, sdo_xsdintf,
  sdo_xsd_helper, xsd_generator, pas_generator;

  procedure LoadSchemaFile(AFactory : ISDODataFactory; AFileName : string);
  var
    xsdHelper : IXSDHelper;
  begin
    try
      xsdHelper := TXSDHelper.Create(AFactory);
      xsdHelper.LoadFromFile(AFileName);
    except
      On E : exception do
        begin
          E.Message:='Load failed'+E.Message;
         raise;

        end;
    end;
  end;

  procedure CreatePascalCode(AFactory : ISDODataFactory; AFileName : string);

  var
    xsdHelper : IXSDHelper;
  begin
    xsdHelper := TXSDHelper.Create(AFactory);
    try
    xsdHelper.GenerateCode(AFactory.getTypes(),'urn:wst-test',AFileName);
    except
      On E : exception do
        begin
          E.Message:='Generate failed'+E.Message;
         raise;

        end;
    end;
  end;


procedure MainProc;

var
  fact : ISDODataFactory;

begin
  Fact:=TSDODataFactory.Create();
  Writeln('Handling ',ParamStr(1));
  LoadSchemaFile(Fact,ParamStr(1));
  CreatePascalCode(Fact,'myunit');
end;


begin
  MainProc;
end.

