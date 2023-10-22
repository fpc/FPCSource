{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2023 by Michael Van Canneyt
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ 
  Tests were made to determine where delphi implementation differs 
  from standards and glscene,  and adapt the implementation
  The numbers of tests done here can also be found in the unit tests.
  
  run this application with number of test to run only that test.
}

{$IFDEF FPC}
{$mode delphi}
uses SysUtils, System.Math.Vectors;
{$ELSE}
uses System.SysUtils, System.Math.Vectors;
{$ENDIF}

{$IFNDEF FPC}
Function MIdentity : TMatrix;

begin
  Result:=TMatrix.Identity;
end;

Function M3DIdentity : TMatrix3D;

begin
  Result:=TMatrix3D.Identity;
end;

// Delphi does not have ToString, use helper to implement it.
// We cannot inherit record helpers in Delphi, 
// so we define this after the above functions to have Identity.  
Type
  TVHelper = record Helper for TVector 
    Function ToString: String;
  end;

  TPHelper = record Helper for TPoint3D 
    Function ToString: String;
  end;

  TMHelper = record Helper {(TMatrixConstants)} for TMatrix 
    function ToString(Multiline: Boolean = True): String; 
    class function Identity : TMatrix; static;
  end;

  TM3DHelper = record Helper {(TMatrixConstants)} for TMatrix3D 
    function ToString(Multiline: Boolean = True): String; 
    class function Identity : TMatrix3D; static;
  end;

  TQ3DHelper = record Helper {(TMatrixConstants)} for TQuaternion3D 
    function ToString(Multiline: Boolean = True): String; 
  end;

Function TVHelper.ToString: String;
begin
  Result:=Format('(%7.4f,%7.4f W:%7.4f)',[X,Y,W]);
end;

Function TPHelper.ToString: String;
begin
  Result:=Format('(%7.4f,%7.4f,%7.4f)',[X,Y,Z]);
end;


class function TMHelper.Identity : TMatrix;
begin
  Result:=Midentity;
end;

function TMHelper.ToString(Multiline: Boolean): String;
var
  S,Sep : String;

begin
  Sep:='';
  if MultiLine then
    Sep:=sLineBreak;
  S:='['+Format('%7.4f,%7.4f,%7.4f',[m11,m12,m13]);
  Result:=S+','+Sep;
  S:=Format('%7.4f,%7.4f,%7.4f',[m21,m22,m23]);
  Result:=Result+S+Sep;
  S:=Format('%7.4f,%7.4f,%7.4f',[m31,m32,m33]);
  Result:=Result+S+']';
end; 

class function TM3DHelper.Identity : TMatrix3D;
begin
  Result:=M3Didentity;
end;

function TM3DHelper.ToString(Multiline: Boolean): String;
var
  S,Sep : String;

begin
  Sep:='';
  if MultiLine then
    Sep:=sLineBreak;
  S:='['+Format('%7.4f,%7.4f,%7.4f,%7.4f',[m11,m12,m13,m14]);
  Result:=S+','+Sep;
  S:=Format('%7.4f,%7.4f,%7.4f,%7.4f',[m21,m22,m23,m24]);
  Result:=Result+S+','+Sep;
  S:=Format('%7.4f,%7.4f,%7.4f,%7.4f',[m31,m32,m33,m34]);
  Result:=Result+S+','+Sep;
  S:=Format('%7.4f,%7.4f,%7.4f,%7.4f',[m41,m42,m43,m44]);
  Result:=Result+S+']';
end; 

 
 function TQ3DHelper.ToString(Multiline: Boolean = True): String; 
 
 begin
   Result:=Format('(%7.4f,i: %7.4f,j: %7.4f, k: %7.4f)',[Realpart,ImagPart.X,ImagPart.Y,ImagPart.Z]);
 end;

{$ENDIF}

Function DoTest(aId : integer; const aCaption : String) : boolean;

var
  cID : Integer;

begin
  cID:=StrToIntDef(Paramstr(1),-1);
  Result:=(aId=cID) or (Cid=-1);
  if Result then
    Writeln('[',aID,'] ',aCaption);
end;

Procedure TV;

var
  V1,V2,V3 : TVector;

begin
  if DoTest(1,'Vector CrossProduct') then
    begin
    V1:=TVector.Create(1,0,0);
    V2:=TVector.Create(0,1,0);
    V3:=V2.CrossProduct(V1);
    With V3 do
      Writeln('1: V3 ',V3.ToString);
    end;
end;

Procedure TP;

var
  P1,P2,P3 : TPoint3D;

begin
  P1:=TPoint3D.Create(1,0,0);
  P2:=TPoint3D.Create(0,0,1); // Z-axis
  if DoTest(101,'Rotate Z axis') then
    begin
    P3:=P1.Rotate(P2,Pi/2);
    Writeln('P3 = ',P3.ToString);
    end;
  if DoTest(102,'Rotate Y axis') then
    begin
    P2:=TPoint3D.Create(0,1,0); // Y-axis
    P3:=P1.Rotate(P2,Pi/2);
    Writeln('P3 = ',P3.ToString);
    end;
end;

Procedure TM;

var
  M,M2 : TMatrix;

begin
  if DoTest(11,'Identity') then
    begin
    M:=TMatrix.Identity;
    Writeln(M.ToString);
    end;
  If DoTest(12,'Rotation (Pi/2)') then
    begin
    M:=TMatrix.CreateRotation(Pi/2);
    Writeln(M.ToString);
    end;
  if DoTest(13,'Rotation Pi') then
    begin
    M:=TMatrix.CreateRotation(Pi);
    Writeln(M.ToString);
    end;
  If DoTest(14,'Scaling 2') then
    begin
    M:=TMatrix.CreateScaling(2,3);
    Writeln(M.ToString);
    end;
  If DoTest(15,'Translation (3,4)') then
    begin
    M:=TMatrix.CreateTranslation(3,4);
    Writeln(M.ToString);
    end;
  If DoTest(16,'Adjoint') then
    begin
    M:=TMatrix.CreateRotation(Pi/6);
    M2:=M.Adjoint;
    Writeln(M2.ToString);
    end;
end;

Procedure Params(const ASource, ATarget, ACeiling: TPoint3D);
var
  ZAxis, XAxis, YAxis: TPoint3D;
begin
  Writeln('aSource: ',aSource.ToString);
  Writeln('aTarget: ',aTarget.ToString);
  Writeln('aCeiling: ',aCeiling.ToString);
 
  ZAxis := (ASource - ATarget).Normalize;
  Writeln('Zaxis: ',ZAxis.ToString);
  XAxis := ACeiling.CrossProduct(ZAxis).Normalize;
  Writeln('Xaxis: ',XAxis.ToString);
  YAxis := ZAxis.CrossProduct(XAxis);
  Writeln('Yaxis: ',YAxis.ToString);
end;

Procedure TM3D;

var
  M,M2,M3 : TMatrix3D;

begin
  if DoTest(31,'M3D.Identity') then
    begin
    M:=TMatrix3D.Identity;
    Writeln(M.ToString);
    end;
  if DoTest(32,'M3D.Rotation ((1,0,0),Pi/2)') then
    begin
    M:=TMatrix3D.CreateRotation(Point3D(1,0,0),Pi/2);
    Writeln(M.ToString);
    end;
  if DoTest(33,'M3D.Rotation ((0,1,0),Pi/2)') then
    begin
    M:=TMatrix3D.CreateRotation(Point3D(0,1,0),Pi/2);
    Writeln(M.ToString);
    end;
  if DoTest(34,'M3D.Rotation ((0,0,1),Pi/2)') then
    begin
    M:=TMatrix3D.CreateRotation(Point3D(0,0,1),pi/2);
    Writeln(M.ToString);
    end;
  if DoTest(35,'M3D.Scaling (1,2,3)') then
    begin
    M:=TMatrix3D.CreateScaling(Point3D(1,2,3));
    Writeln(M.ToString);
    end;
  if DoTest(36,'MD3D.Translation (3,4,5)') then
    begin
    M:=TMatrix3d.CreateTranslation(Point3D(3,4,5));
    Writeln(M.ToString);
    end;
  if DoTest(37,'M3D.HeadingPitchBank(Pi/2,0,0)') then
    begin
    M:=TMatrix3D.CreateRotationHeadingPitchBank(Pi/2,0,0);
    Writeln(M.ToString);
    end;
  if DoTest(38,'M3D.HeadingPitchBank(0,Pi/2,0)') then
    begin
    M:=TMatrix3D.CreateRotationHeadingPitchBank(0,Pi/2,0);
    Writeln(M.ToString);
    end;
  if DoTest(39,'M3D.HeadingPitchBank(0,0,Pi/2)') then
    begin
    M:=TMatrix3D.CreateRotationHeadingPitchBank(0,0,Pi/2);
    Writeln(M.ToString);
    end;
  if DoTest(40,'M3D.RotationX(Pi/2)') then
    begin
    M:=TMatrix3D.CreateRotationX(Pi/2);
    Writeln(M.ToString);
    end;
  if DoTest(41,'M3D.RotationY(Pi/2)') then
    begin
    M:=TMatrix3D.CreateRotationY(Pi/2);
    Writeln(M.ToString);
    end;
  if DoTest(42,'M3D.RotationZ(Pi/2)') then
    begin
    M:=TMatrix3D.CreateRotationZ(Pi/2);
    Writeln(M.ToString);
    end;
  if DoTest(43,'M3D.YawPitchRoll(Pi/2,0,0)') then
    begin
    M:=TMatrix3D.CreateRotationYawPitchRoll(Pi/2,0,0);
    Writeln(M.ToString);
    end;
  if DoTest(44,'M3D.YawPitchRoll(0,Pi/2,0)') then
    begin
    M:=TMatrix3D.CreateRotationYawPitchRoll(0,Pi/2,0);
    Writeln(M.ToString);
    end;
  if DoTest(45,'M3D.YawPitchRoll(0,0,Pi/2)') then
    begin
    M:=TMatrix3D.CreateRotationYawPitchRoll(0,0,Pi/2);
    Writeln(M.ToString);
    end;
  if DoTest(46,'M3D.LookAtDirLH((0,0,0),(0,0,1),(0,0,1)') then
    begin
    M:=TMatrix3D.CreateLookAtDirLH(Point3D(0,0,0),Point3D(0,0,1),Point3D(0,0,1));
    Writeln(M.ToString);
    end;
  if DoTest(47,'M3D.LookAtDirLH((0,0,0),(0,1,0),(0,0,1)') then
    begin
    M:=TMatrix3D.CreateLookAtDirLH(Point3D(0,0,0),Point3D(0,1,0),Point3D(0,0,1));
    Writeln(M.ToString);
    end;
  if DoTest(48,'M3D.LookAtDirLH((0,0,0),(1,0,0),(0,0,1)') then
    begin
    M:=TMatrix3D.CreateLookAtDirLH(Point3D(0,0,0),Point3D(1,0,0),Point3D(0,0,1));
    Writeln(M.ToString);
    end;
  if DoTest(49,'M3D.LookAtDirRH((0,0,0),(0,0,1),(0,0,1)') then
    begin
    M:=TMatrix3D.CreateLookAtDirRH(Point3D(0,0,0),Point3D(0,0,1),Point3D(0,0,1));
    Writeln(M.ToString);
    end;
  if DoTest(50,'M3D.LookAtDirRH((0,0,0),(0,1,0),(0,0,1)') then
    begin
    M:=TMatrix3D.CreateLookAtDirRH(Point3D(0,0,0),Point3D(0,1,0),Point3D(0,0,1));
    Writeln(M.ToString);
    end;
  if DoTest(51,'M3D.LookAtDirRH((0,0,0),(1,0,0),(0,0,1)') then
    begin
    M:=TMatrix3D.CreateLookAtDirRH(Point3D(0,0,0),Point3D(1,0,0),Point3D(0,0,1));
    Writeln(M.ToString);
    end;

  if DoTest(52,'M3D.LookAtLH((0,0,0),(0,0,1),(0,0,1)') then
    begin
    M:=TMatrix3D.CreateLookAtLH(Point3D(0,0,0),Point3D(0,0,1),Point3D(0,0,1));
    Writeln(M.ToString);
    end;
  if DoTest(53,'M3D.LookAtLH((0,0,0),(0,1,0),(0,0,1)') then
    begin
    M:=TMatrix3D.CreateLookAtLH(Point3D(0,0,0),Point3D(0,1,0),Point3D(0,0,1));
    Writeln(M.ToString);
    end;
  if DoTest(54,'M3D.LookAtLH((0,0,0),(1,0,0),(0,0,1)') then
    begin
    M:=TMatrix3D.CreateLookAtLH(Point3D(0,0,0),Point3D(1,0,0),Point3D(0,0,1));
    Writeln(M.ToString);
    end;
  if DoTest(55,'M3D.LookAtRH((0,0,0),(0,0,1),(0,0,1)') then
    begin
    Params(Point3D(0,0,0),Point3D(0,0,1),Point3D(0,0,1));
    M:=TMatrix3D.CreateLookAtRH(Point3D(0,0,0),Point3D(0,0,1),Point3D(0,0,1));
    Writeln(M.ToString);
    end;
  if DoTest(56,'M3D.LookAtRH((0,0,0),(0,1,0),(0,0,1)') then
    begin
    M:=TMatrix3D.CreateLookAtRH(Point3D(0,0,0),Point3D(0,1,0),Point3D(0,0,1));
    Writeln(M.ToString);
    end;
  if DoTest(57,'M3D.LookAtRH((0,0,0),(1,0,0),(0,0,1)') then
    begin
    M:=TMatrix3D.CreateLookAtRH(Point3D(0,0,0),Point3D(1,0,0),Point3D(0,0,1));
    Writeln(M.ToString);
    end;
  if DoTest(58,'M3D.CreateOrthoLH(1,1,0,1)') then
    begin
    M:=TMatrix3D.CreateOrthoLH(1,1,0,1);
    Writeln(M.ToString);
    end;
  if DoTest(59,'M3D.CreateOrthoLH(1,1,0,2)') then
    begin
    M:=TMatrix3D.CreateOrthoLH(1,1,0,2);
    Writeln(M.ToString);
    end;
  if DoTest(60,'M3D.CreateOrthoRH(1,1,0,1)') then
    begin
    M:=TMatrix3D.CreateOrthoRH(1,1,0,1);
    Writeln(M.ToString);
    end;
  if DoTest(61,'M3D.CreateOrthoRH(1,1,0,2)') then
    begin
    M:=TMatrix3D.CreateOrthoRH(1,1,0,2);
    Writeln(M.ToString);
    end;
  if DoTest(62,'M3D.CreateOrthoOffCenterLH(0,0,1,1,0,1)') then
    begin
    M:=TMatrix3D.CreateOrthoOffCenterLH(0,0,1,1,0,1);
    Writeln(M.ToString);
    end;
  if DoTest(63,'M3D.CreateOrthoOffCenterLH(0,0,1,1,0,2)') then
    begin
    M:=TMatrix3D.CreateOrthoOffCenterLH(0,0,1,1,0,2);
    Writeln(M.ToString);
    end;
  if DoTest(64,'M3D.CreateOrthoOffCenterRH(0,0,1,1,0,1)') then
    begin
    M:=TMatrix3D.CreateOrthoOffCenterRH(0,0,1,1,0,1);
    Writeln(M.ToString);
    end;
  if DoTest(65,'M3D.CreateOrthoOffCenterRH(0,0,1,1,0,2)') then
    begin
    M:=TMatrix3D.CreateOrthoOffCenterRH(0,0,1,1,0,2);
    Writeln(M.ToString);
    end;
  if DoTest(66,'M3D.CreatePerspectiveFovLH(pi/2,1,0,1)') then
    begin
    M:=TMatrix3D.CreatePerspectiveFovLH(pi/2,1,0,1);
    Writeln(M.ToString);
    end;
  if DoTest(67,'M3D.CreatePerspectiveFovLH(pi/21,0,1,True)') then
    begin
    M:=TMatrix3D.CreatePerspectiveFovLH(pi/2,1,0,1,True);
    Writeln(M.ToString);
    end;
  if DoTest(68,'M3D.CreatePerspectiveFovRH(pi/2,1,0,1)') then
    begin
    M:=TMatrix3D.CreatePerspectiveFovRH(pi/2,1,0,1);
    Writeln(M.ToString);
    end;
  if DoTest(69,'M3D.CreatePerspectiveFovRH(pi/2,1,0,1,True)') then
    begin
    M:=TMatrix3D.CreatePerspectiveFovRH(pi/2,1,0,1,True);
    Writeln(M.ToString);
    end;
  if DoTest(70,'M3D.Create(array)') then
    begin
    M:=TMatrix3D.Create([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]);
    Writeln(M.ToString);
    end;
  if DoTest(71,'M3D.CreateScaling(Point3D(1,2,3))') then
    begin
    M:=TMatrix3D.CreateScaling(Point3D(1,2,3));
    Writeln(M.ToString);
    end;
  if DoTest(72,'M3D.CreateTranslation(Point3D(1,2,3))') then
    begin
    M:=TMatrix3D.CreateTranslation(Point3D(1,2,3));
    Writeln(M.ToString);
    end;
  if DoTest(73,'M3D*M3D') then
    begin
    M:=TMatrix3D.Create( [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]);
    M2:=TMatrix3D.Create([16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]);
    M3:=M*M2;
    Writeln(M3.ToString);
    end;
  if DoTest(74,'M3D.Adjoint') then
    begin
    M:=TMatrix3D.CreateRotationZ(Pi/3);
    M2:=M.Adjoint;
    Writeln(M2.ToString);
    end;
  if DoTest(75,'M3D.Determinant') then
    begin
    M:=TMatrix3D.CreateRotationZ(Pi/3);
    Writeln(M.Determinant);
    end;
  if DoTest(76,'M3D.Determinant') then
    begin
    M:=TMatrix3D.Create(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16);
//    M:=TMatrix3D.Create(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1);
    Writeln(M.Determinant);
    end;
  if DoTest(77,'M3D.Rotation.EyePosition') then
    begin
    M:=TMatrix3D.CreateRotationZ(Pi/2);
    Writeln(M.EyePosition.ToString);
    end;
  if DoTest(78,'M3D.Translation.EyePosition') then
    begin
    M:=TMatrix3D.CreateRotationZ(Pi/3)*TMatrix3D.CreateTranslation(Point3D(1,0,0));
    Writeln(M.EyePosition.ToString);
    end;
  if DoTest(79,'M3D.Inverse') then
    begin
    M:=TMatrix3D.CreateRotationZ(Pi/3);
    Writeln(M.Inverse.ToString);
    end;
      
    
end;

Procedure TQ;

var
  Q1,Q2 : TQuaternion3D;

begin
{$IFDEF FPC}
  If DoTest(201,'TQuaternion3D.Create(Point3D(1,0,0),Point3D(0,1,0))') then
    begin
    Q1:=TQuaternion3D.Create(Point3D(1,0,0),Point3D(0,1,0));
    Writeln('Q = ',Q1.ToString);
    end;
{$ENDIF}    
  If DoTest(202,'TQuaternion3D.Create(Point3D(1,0,0),Pi/2)') then
    begin
    Q1:=TQuaternion3D.Create(Point3D(1,0,0),Pi/2);
    Writeln('Q = ',Q1.ToString);
    end;
  If DoTest(203,'TQuaternion3D.Create(Point3D(0,1,0),Pi/2)') then
    begin
    Q1:=TQuaternion3D.Create(Point3D(0,1,0),Pi/2);
    Writeln('Q = ',Q1.ToString);
    end;
  If DoTest(204,'TQuaternion3D.Create(Point3D(0,0,1),Pi/2)') then
    begin
    Q1:=TQuaternion3D.Create(Point3D(0,0,1),Pi/2);
    Writeln('Q = ',Q1.ToString);
    end;
  If DoTest(205,'TQuaternion3D.Create(Pi/2,0,0)') then
    begin
    Q1:=TQuaternion3D.Create(Pi/2,0,0);
    Writeln('Q = ',Q1.ToString);
    end;
  If DoTest(206,'TQuaternion3D.Create(0,Pi/2,0)') then
    begin
    Q1:=TQuaternion3D.Create(0,Pi/2,0);
    Writeln('Q = ',Q1.ToString);
    end;
  If DoTest(207,'TQuaternion3D.Create(0,0,Pi/2)') then
    begin
    Q1:=TQuaternion3D.Create(0,0,Pi/2);
    Writeln('Q = ',Q1.ToString);
    end;
  If DoTest(208,'TQuaternion3D.Multiply (yawpitchroll)') then
    begin
    Q1:=TQuaternion3D.Create(0,0,Pi/2);
    Q2:=TQuaternion3D.Create(0,Pi/2,0);
    Writeln('Q1*Q2 = ',(Q1*Q2).ToString);
    end;
  If DoTest(209,'TQuaternion3D.Multiply (angle/vector)') then
    begin
    Q1:=TQuaternion3D.Create(Point3D(1,0,0),Pi/2);
    Q2:=TQuaternion3D.Create(Point3D(0,1,0),Pi/2);
    Writeln('Q1*Q2 = ',(Q1*Q2).ToString);
    end;

end;

begin
  TV;
  TP;
  TM;
  TM3D;
  TQ;
end.
