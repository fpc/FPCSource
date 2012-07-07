{%norun}
program tw21941;

{$mode objfpc}{$H+}

type
  TACLInfo = record
    Name: string;
  end;
  PACLInfo = ^TACLInfo;

  TCLSInfo = record
    Name: string;
  end;
  PCLSInfo = ^TCLSInfo;

  TCoreObjectInfo = record
    CLSInfo: PCLSInfo;
    ACLInfo: PACLInfo;
  end;

type
  Root = class
  type
    Test = class
    const
      ACLInfo: TACLInfo = (
        Name: 'Admin';
      );
      CLSInfo: TCLSInfo = (
        Name: 'TAdminCore';
      );
      Header: TCoreObjectInfo = (
        CLSInfo: @CLSInfo;
        ACLInfo: @ACLInfo;
      );
    end;
  end;
begin
end.

