program demo_classes;
{$mode delphi}{$H+}
{$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}

uses
  SysUtils, TypInfo, Rtti,
  System.JSON.Types,
  System.JSON.Serializers;

type
  { Simple class with published properties }
  TAnimal = class
  private
    FName: string;
    FSpecies: string;
    FLegs: Integer;
    FDomesticated: Boolean;
  published
    property Name: string read FName write FName;
    property Species: string read FSpecies write FSpecies;
    property Legs: Integer read FLegs write FLegs;
    property Domesticated: Boolean read FDomesticated write FDomesticated;
  end;

  { Class with different field types }
  TProduct = class
  private
    FId: Integer;
    FTitle: string;
    FPrice: Double;
    FInStock: Boolean;
    FDescription: string;
  published
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property Price: Double read FPrice write FPrice;
    property InStock: Boolean read FInStock write FInStock;
    property Description: string read FDescription write FDescription;
  end;

  { Nested class composition }
  TEngine = record
    Horsepower: Integer;
    FuelType: string;
  end;

  TCar = record
    Make: string;
    Model: string;
    Year: Integer;
    Engine: TEngine;
  end;

var
  Ser: TJsonSerializer;

  procedure Section(const Title: string);
  begin
    WriteLn;
    WriteLn('=== ', Title, ' ===');
  end;

  procedure DemoSimpleClass;
  var
    A, A2: TAnimal;
    Json: string;
  begin
    Section('Simple Class Serialization');

    A := TAnimal.Create;
    try
      A.Name := 'Rex';
      A.Species := 'Dog';
      A.Legs := 4;
      A.Domesticated := True;

      Json := Ser.Serialize<TAnimal>(A);
      WriteLn('  Serialized TAnimal:');
      WriteLn('    ', Json);
    finally
      A.Free;
    end;

    WriteLn;
    WriteLn('  Deserialized from JSON:');
    A2 := Ser.Deserialize<TAnimal>(Json);
    try
      WriteLn('    Name=', A2.Name, ', Species=', A2.Species,
              ', Legs=', A2.Legs, ', Domesticated=', A2.Domesticated);
    finally
      A2.Free;
    end;
  end;

  procedure DemoProductClass;
  var
    P: TProduct;
    Json: string;
  begin
    Section('Class with Mixed Property Types');

    P := TProduct.Create;
    try
      P.Id := 1001;
      P.Title := 'Widget Pro';
      P.Price := 29.99;
      P.InStock := True;
      P.Description := 'A premium widget with "advanced" features';

      Json := Ser.Serialize<TProduct>(P);
      WriteLn('  Serialized TProduct:');
      WriteLn('    ', Json);
    finally
      P.Free;
    end;

    WriteLn;
    P := Ser.Deserialize<TProduct>(Json);
    try
      WriteLn('  Round-trip verification:');
      WriteLn('    Id=', P.Id, ', Title=', P.Title);
      WriteLn('    Price=', P.Price:0:2, ', InStock=', P.InStock);
      WriteLn('    Description=', P.Description);
    finally
      P.Free;
    end;
  end;

  procedure DemoNilHandling;
  var
    A: TAnimal;
    Json: string;
  begin
    Section('Nil Object Handling');

    A := nil;
    Json := Ser.Serialize<TAnimal>(A);
    WriteLn('  Serialized nil TAnimal: ', Json);

    A := Ser.Deserialize<TAnimal>('null');
    WriteLn('  Deserialized "null": ', IntToHex(PtrUInt(A), 1), ' (nil)');
    WriteLn('  A = nil? ', A = nil);
  end;

  procedure DemoRecordComposition;
  var
    C, C2: TCar;
    Json: string;
  begin
    Section('Record Composition (Nested Records)');

    C.Make := 'Toyota';
    C.Model := 'Camry';
    C.Year := 2024;
    C.Engine.Horsepower := 203;
    C.Engine.FuelType := 'Gasoline';

    Json := Ser.Serialize<TCar>(C);
    WriteLn('  Serialized TCar:');
    WriteLn('    ', Json);

    C2 := Ser.Deserialize<TCar>(Json);
    WriteLn;
    WriteLn('  Deserialized:');
    WriteLn('    Make=', C2.Make, ', Model=', C2.Model, ', Year=', C2.Year);
    WriteLn('    Engine: ', C2.Engine.Horsepower, 'hp, ', C2.Engine.FuelType);
  end;

  procedure DemoFieldNaming;
  var
    Resolver: TJsonDynamicContractResolver;
    SerR: TJsonSerializer;
    A: TAnimal;
  begin
    Section('Class Fields & Dynamic Renaming');

    A := TAnimal.Create;
    try
      A.Name := 'Whiskers';
      A.Species := 'Cat';
      A.Legs := 4;
      A.Domesticated := True;

      WriteLn('  Default (MemberSerialization=Fields, shows private fields):');
      WriteLn('    ', Ser.Serialize<TAnimal>(A));
      WriteLn;
      WriteLn('  Note: Private fields have "F" prefix (FName, FSpecies, etc.).');
      WriteLn('  Use TJsonDynamicContractResolver to rename them at runtime:');

      Resolver := TJsonDynamicContractResolver.Create;
      SerR := TJsonSerializer.Create;
      try
        Resolver.SetFieldName(TypeInfo(TAnimal), 'FName', 'name');
        Resolver.SetFieldName(TypeInfo(TAnimal), 'FSpecies', 'species');
        Resolver.SetFieldName(TypeInfo(TAnimal), 'FLegs', 'legs');
        Resolver.SetFieldName(TypeInfo(TAnimal), 'FDomesticated', 'domesticated');
        SerR.ContractResolver := Resolver;

        WriteLn;
        WriteLn('  With dynamic renaming (F-prefix removed):');
        WriteLn('    ', SerR.Serialize<TAnimal>(A));
      finally
        SerR.Free;
      end;
    finally
      A.Free;
    end;
  end;

begin
  Ser := TJsonSerializer.Create;
  try
    WriteLn('Demo: Class Serialization');
    WriteLn('=========================');

    DemoSimpleClass;
    DemoProductClass;
    DemoNilHandling;
    DemoRecordComposition;
    DemoFieldNaming;

    WriteLn;
    WriteLn('Done.');
  finally
    Ser.Free;
  end;
end.
