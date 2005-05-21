{
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
        procedure AddSection(const s:string);
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


    procedure TPackageFpcWriter.AddSection(const s:string);
      var
        Sec : TFPCMakeSection;
      begin
        Sec:=TFPCMakeSection(FInput[s]);
        if assigned(Sec) then
         begin
           Sec.BuildIni;
           FOutput.Add('['+s+']');
           FOutput.AddStrings(Sec.List);
         end;
      end;


    procedure TPackageFpcWriter.WritePackageFpc;
      begin
        { Only write the Package.fpc if the package is
          section available }
        if not assigned(FInput['package']) then
         begin
           FInput.Verbose(FPCMakeInfo,'Not writing Package.fpc, no package section');
           exit;
         end;

        { Generate Output }
        with FOutput do
         begin
           AddSection('package');
           AddSection('require');
         end;

        { write to disk }
        FInput.Verbose(FPCMakeInfo,'Writing Package.fpc');
        FOutput.SaveToFile(FFileName);
      end;

end.
