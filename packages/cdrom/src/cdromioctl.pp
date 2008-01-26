unit cdromioctl;
{$mode objfpc}
interface

uses windows;

{
  Automatically converted by H2Pas 0.99.15 from cdromioctl.h
  The following command line parameters were used:
    cdromioctl.h
}

{$PACKRECORDS C}

  {
     distilled information from various header files from Microsoft's
     DDK for Windows NT 4.0
  }

  type

     SCSI_PASS_THROUGH = record
          Length : USHORT;
          ScsiStatus : UCHAR;
          PathId : UCHAR;
          TargetId : UCHAR;
          Lun : UCHAR;
          CdbLength : UCHAR;
          SenseInfoLength : UCHAR;
          DataIn : UCHAR;
          DataTransferLength : ULONG;
          TimeOutValue : ULONG;
          DataBufferOffset : ULONG;
          SenseInfoOffset : ULONG;
          Cdb : array[0..15] of UCHAR;
       end;
     PSCSI_PASS_THROUGH = ^SCSI_PASS_THROUGH;

     SCSI_PASS_THROUGH_DIRECT = record
          Length : USHORT;
          ScsiStatus : UCHAR;
          PathId : UCHAR;
          TargetId : UCHAR;
          Lun : UCHAR;
          CdbLength : UCHAR;
          SenseInfoLength : UCHAR;
          DataIn : UCHAR;
          DataTransferLength : ULONG;
          TimeOutValue : ULONG;
          DataBuffer : PVOID;
          SenseInfoOffset : ULONG;
          Cdb : array[0..15] of UCHAR;
       end;
     PSCSI_PASS_THROUGH_DIRECT = ^SCSI_PASS_THROUGH_DIRECT;

     SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER = record
          spt : SCSI_PASS_THROUGH_DIRECT;
          Filler : ULONG;
          ucSenseBuf : array[0..31] of UCHAR;
       end;
     PSCSI_PASS_THROUGH_DIRECT_WITH_BUFFER = ^SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER;
  {
     method codes
    }

  const
     METHOD_BUFFERED = 0;
     METHOD_IN_DIRECT = 1;
     METHOD_OUT_DIRECT = 2;
     METHOD_NEITHER = 3;
  {
     file access values
    }
     FILE_ANY_ACCESS = 0;
     FILE_READ_ACCESS = $0001;
     FILE_WRITE_ACCESS = $0002;
     IOCTL_CDROM_BASE = $00000002;
     IOCTL_SCSI_BASE = $00000004;
  {
     constants for DataIn member of SCSI_PASS_THROUGH  structures
    }
     SCSI_IOCTL_DATA_OUT = 0;
     SCSI_IOCTL_DATA_IN = 1;
     SCSI_IOCTL_DATA_UNSPECIFIED = 2;

    {
     Standard IOCTL define
    }

    Function CTL_CODE( ADevType, AFunction, AMethod, AAccess : Longint) : Longint;
    function IOCTL_CDROM_READ_TOC : Longint;
    function IOCTL_CDROM_GET_LAST_SESSION : longint;
    function IOCTL_SCSI_PASS_THROUGH : longint;
    function IOCTL_SCSI_MINIPORT : longint;
    function IOCTL_SCSI_GET_INQUIRY_DATA : longint;
    function IOCTL_SCSI_GET_CAPABILITIES : longint;
    function IOCTL_SCSI_PASS_THROUGH_DIRECT : longint;
    function IOCTL_SCSI_GET_ADDRESS : longint;


implementation

    { was #define dname def_expr }
    function IOCTL_CDROM_GET_LAST_SESSION : longint;
        { return type might be wrong }
        begin
           IOCTL_CDROM_GET_LAST_SESSION:=CTL_CODE(IOCTL_CDROM_BASE,$000E,METHOD_BUFFERED,FILE_READ_ACCESS);
        end;

    { was #define dname def_expr }
    function IOCTL_SCSI_PASS_THROUGH : longint;
        { return type might be wrong }
        begin
           IOCTL_SCSI_PASS_THROUGH:=CTL_CODE(IOCTL_SCSI_BASE,$0401,METHOD_BUFFERED,FILE_READ_ACCESS or FILE_WRITE_ACCESS);
        end;

    { was #define dname def_expr }
    function IOCTL_SCSI_MINIPORT : longint;
        { return type might be wrong }
        begin
           IOCTL_SCSI_MINIPORT:=CTL_CODE(IOCTL_SCSI_BASE,$0402,METHOD_BUFFERED,FILE_READ_ACCESS or FILE_WRITE_ACCESS);
        end;

    { was #define dname def_expr }
    function IOCTL_SCSI_GET_INQUIRY_DATA : longint;
        { return type might be wrong }
        begin
           IOCTL_SCSI_GET_INQUIRY_DATA:=CTL_CODE(IOCTL_SCSI_BASE,$0403,METHOD_BUFFERED,FILE_ANY_ACCESS);
        end;

    { was #define dname def_expr }
    function IOCTL_SCSI_GET_CAPABILITIES : longint;
        { return type might be wrong }
        begin
           IOCTL_SCSI_GET_CAPABILITIES:=CTL_CODE(IOCTL_SCSI_BASE,$0404,METHOD_BUFFERED,FILE_ANY_ACCESS);
        end;

    { was #define dname def_expr }
    function IOCTL_SCSI_PASS_THROUGH_DIRECT : longint;
        { return type might be wrong }
        begin
           IOCTL_SCSI_PASS_THROUGH_DIRECT:=CTL_CODE(IOCTL_SCSI_BASE,$0405,METHOD_BUFFERED,FILE_READ_ACCESS or FILE_WRITE_ACCESS);
        end;

    { was #define dname def_expr }
    function IOCTL_SCSI_GET_ADDRESS : longint;
        { return type might be wrong }
        begin
           IOCTL_SCSI_GET_ADDRESS:=CTL_CODE(IOCTL_SCSI_BASE,$0406,METHOD_BUFFERED,FILE_ANY_ACCESS);
        end;

Function CTL_CODE( ADevType, AFunction, AMethod, AAccess : Longint) : Longint;

begin
    Result:= (ADevType shl 16 )
             Or (AAccess shl 14)
             Or (AFunction SHL 2)
             Or AMethod;
end;

function IOCTL_CDROM_READ_TOC : Longint;

begin
  Result:=CTL_CODE(IOCTL_CDROM_BASE, 0, METHOD_BUFFERED, FILE_READ_ACCESS )
end;


end.
