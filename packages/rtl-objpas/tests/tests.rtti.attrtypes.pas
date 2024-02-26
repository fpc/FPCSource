unit tests.rtti.attrtypes;

{These types are put in a different unit so the $RTTI directive only influences these classes }

{$mode objfpc}
{$ModeSwitch prefixedattributes}
{$modeswitch advancedrecords}

interface

uses
  TypInfo;

Type
   {$RTTI Explicit Fields[vcPrivate,vcPublic,vcProtected,vcPublished]
                   Properties[vcPrivate,vcPublic,vcProtected,vcPublished]
   }

   { TIntAttribute }

   { WeakAttribute }

   WeakAttribute = class(TCustomAttribute)
     Constructor Create;
   end;

   TIntAttribute = class(TCustomAttribute)
   Private
     FInt : Integer;
   Public
     Constructor Create(aInt: Integer);
     Property Int : Integer Read FInt;
   end;

   MyAttribute = class(TIntAttribute)
   end;

   My2Attribute = class(TIntAttribute);
   My3Attribute = class(TIntAttribute);
   My4Attribute = class(TIntAttribute);


   TFieldObject = Class(TObject)
   Private
     [Weak]
     [my(1)]
     [my2(2)]
     PrivateField : Integer;
   Protected
     [my2(3)]
     ProtectedField : Integer;
   Public
     [my3(4)]
     PublicField : Integer;
   Public
     [my3(4)]
     A, B : Integer;
   end;

   {$M+}
   TPropertyObject = Class(TObject)
   Private
     PrivateField : Integer;
     [Weak]
     [my(1)]
     [my2(2)]
     Property PrivateProperty : Integer Read PrivateField;
   Protected
     ProtectedField : Integer;
     [my2(3)]
     Property ProtectedProperty : Integer Read ProtectedField;
   Public
     PublicField : Integer;
     PublishedField : Integer;
     [my3(4)]
     Property PublicProperty : Integer Read PublicField;
   Published
     [my3(5)]
     Property PublishedProperty : Integer Read PublishedField;
   end;

   TFieldRecord = Record
   Private
     [Weak]
     [my(1)]
     [my2(2)]
     PrivateField : Integer;
   Public
     [my3(3)]
     PublicField : Integer;
   Public
     [my3(4)]
     A,B : Integer;
   end;


implementation


constructor WeakAttribute.Create;
begin
  // Do nothing
end;

{ TIntAttribute }

constructor TIntAttribute.Create(aInt: Integer);
begin
  Fint:=aInt;
end;


end.

