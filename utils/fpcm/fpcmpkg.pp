{
    $Id$
    Copyright (c) 2001 by Peter Vreman

    FPCMake - Package.Fpc writer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef fpc}{$mode objfpc}{$endif}
{$H+}
unit fpcmpkg;
interface

    uses
      sysutils,classes,
      fpcmmain;

    type
      TPackageFpcWriter=class
      private
        FFileName : string;
        FInput  : TFPCMake;
        FOutput : TStringList;
      public
        constructor Create(AFPCMake:TFPCMake;const AFileName:string);
        destructor  Destroy;override;
        procedure WritePackageFpc;
      end;


implementation

{*****************************************************************************
                               Helpers
*****************************************************************************}

    function FixVariable(s:string):string;
      var
        i : integer;
      begin
        Result:=UpperCase(s);
        i:=pos('.',Result);
        if i>0 then
         Result[i]:='_';
      end;


{*****************************************************************************
                          TPackageFpcWriter
*****************************************************************************}

    constructor TPackageFpcWriter.Create(AFPCMake:TFPCMake;const AFileName:string);
      begin
        FInput:=AFPCMake;
        FFileName:=AFileName;
        FOutput:=TStringList.Create;
      end;


    destructor TPackageFpcWriter.Destroy;
      begin
        FOutput.Free;
      end;


    procedure TPackageFpcWriter.WritePackageFpc;
      begin
        FInput.Print;

        { Generate Output }
        with FOutput do
         begin
         end;
        { write to disk }
        FOutput.SaveToFile(FFileName);
      end;

end.
{
  $Log$
  Revision 1.1  2001-06-04 21:42:57  peter
    * Arguments added
    * Start of Package.fpc creation

}
