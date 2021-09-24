unit urtticl;

{$mode ObjFPC}{$H+}
{$M+}

interface

uses
  Classes, SysUtils;


Type


{$RTTI EXPLICIT
      PROPERTIES([vcProtected,vcPublished])
      FIELDS([vcProtected,vcPublished])
      METHODS([vcProtected,vcPublished])}

   { TProtectedPublishedFieldRTTI }

   TProtectedPublishedFieldRTTI = Class
   private
     FPrivateA: Integer;
     Property PrivateA : Integer Read FPrivateA Write FPrivateA;
   strict private
     FPrivateB: Integer;
     Property PrivateB : Integer Read FPrivateB Write FPrivateB;
   Protected
     FProtectedA: Integer;
     Property ProtectedA : Integer Read FProtectedA Write FProtectedA;
   Strict Protected
     FProtectedB: Integer;
     Property ProtectedB : Integer Read FProtectedB Write FProtectedB;
   Public
     FPublicA: Integer;
     FPublicB: Integer;
     Property PublicA : Integer Read FPublicA Write FPublicA;
     Property PublicB : Integer Read FPublicA Write FPublicB;
   Published
     FPublishedA: Integer;
     FPublishedB: Integer;
     Property PublishedA : Integer Read FPublishedA Write FPublishedA;
     Property PublishedB : Integer Read FPublishedA Write FPublishedB;
   end;

   { TProtectedPublishedMethodClassRTTI }

   TProtectedPublishedMethodClassRTTI = Class (TObject)
   private
     Procedure PrivateMethodA;
   strict private
     Procedure PrivateMethodB; virtual;
   private
     Procedure PrivateMethodC; virtual; abstract;
   protected
     Procedure ProtectedMethodA;
   strict protected
     Procedure ProtectedMethodB; virtual;
   protected
     Procedure ProtectedMethodC; virtual; abstract;
   public
     Procedure PublicMethodA;
     Procedure PublicMethodB; virtual;
     Procedure PublicMethodC; virtual; abstract;
   published
     Procedure PublishedMethodA;
     Procedure PublishedMethodB; virtual;
     Procedure PublishedMethodC; virtual; abstract;
   end;


implementation

{ TPrivatePublicMethodClassRTTI }


{ TProtectedPublishedMethodClassRTTI }

procedure TProtectedPublishedMethodClassRTTI.PrivateMethodA;
begin

end;

procedure TProtectedPublishedMethodClassRTTI.PrivateMethodB;
begin

end;

procedure TProtectedPublishedMethodClassRTTI.ProtectedMethodA;
begin

end;

procedure TProtectedPublishedMethodClassRTTI.ProtectedMethodB;
begin

end;

procedure TProtectedPublishedMethodClassRTTI.PublicMethodA;
begin

end;

procedure TProtectedPublishedMethodClassRTTI.PublicMethodB;
begin

end;

procedure TProtectedPublishedMethodClassRTTI.PublishedMethodA;
begin

end;

procedure TProtectedPublishedMethodClassRTTI.PublishedMethodB;
begin

end;

(*
{$RTTI EXPLICIT
   PROPERTIES([vcPrivate,vcPublic])
   FIELDS([vcPrivate,vcPublic])
   METHODS([vcPrivate,vcPublic])}
*)

{ TFieldRTTI }
type
TFieldRTTI = Class
private
  FPrivateA: Integer;
  Property PrivateA : Integer Read FPrivateA Write FPrivateA;
strict private
  FPrivateB: Integer;
  Property PrivateB : Integer Read FPrivateB Write FPrivateB;
Protected
  FProtectedA: Integer;
  Property ProtectedA : Integer Read FProtectedA Write FProtectedA;
Strict Protected
  FProtectedB: Integer;
  Property ProtectedB : Integer Read FProtectedB Write FProtectedB;
Public
  FPublicA: Integer;
  FPublicB: Integer;
  Property PublicA : Integer Read FPublicA Write FPublicA;
  Property PublicB : Integer Read FPublicA Write FPublicB;
end;

{ TMethodClassRTTI }

TMethodClassRTTI = Class (TObject)
private
  Procedure PrivateMethodA;
strict private
  Procedure PrivateMethodB; virtual;
private
  Procedure PrivateMethodC; virtual; abstract;
protected
  Procedure ProtectedMethodA;
strict protected
  Procedure ProtectedMethodB; virtual;
protected
  Procedure ProtectedMethodC; virtual; abstract;
public
  Procedure PublicMethodA;
  Procedure PublicMethodB; virtual;
  Procedure PublicMethodC; virtual; abstract;
published
  Procedure PublishedMethodA;
  Procedure PublishedMethodB; virtual;
  Procedure PublishedMethodC; virtual; abstract;
end;

{ TMethodClassRTTI }

procedure TMethodClassRTTI.PrivateMethodA;
begin

end;

procedure TMethodClassRTTI.PrivateMethodB;
begin

end;

procedure TMethodClassRTTI.ProtectedMethodA;
begin

end;

procedure TMethodClassRTTI.ProtectedMethodB;
begin

end;

procedure TMethodClassRTTI.PublicMethodA;
begin

end;

procedure TMethodClassRTTI.PublicMethodB;
begin

end;

procedure TMethodClassRTTI.PublishedMethodA;
begin

end;

procedure TMethodClassRTTI.PublishedMethodB;
begin

end;


end.

