{ %version=1.1 }
{$mode objfpc}
{ Source provided for Free Pascal Bug Report 2865 }
{ Submitted by "Eric Grange" on  2003-12-30 }
{ e-mail: egrange@glscene.org }
unit tw2865;

interface

uses Classes;

type
   TVector = array [0..3] of Single;

   TGLUpdateAbleObject = class (TPersistent)
   end;

   TGLCoordinates = class (TGLUpdateAbleObject)
      private
         { Private Declarations }
         FCoords : TVector;

      published
         { Published Declarations }
         property X : Single index 0 read FCoords[0];
   end;

implementation

end.
