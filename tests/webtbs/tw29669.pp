{$mode objfpc}

program Project1;

uses
 SysUtils;

type
  TPackedIdLevel1 = 0..255;
  TPackedIdLevel2 = 0..65535;
  TPackedIdLevel3 = 0..65535;
  TPackedIdLevel4 = 0..65535;
  TPackedIdLevel5 = 0..255;

  TPackedId = bitpacked record
    clusterId : TPackedIdLevel5;
    agentId : TPackedIdLevel4;
    dataSourceId : TPackedIdLevel3;
    deviceId : TPackedIdLevel2;
    esmId : TPackedIdLevel1;
  end;

function PackedIdToStr(const ipsid : qword) : string;
begin
  result := IntToStr(TPackedId(ipsid).esmId) + '-' +
            IntToStr(TPackedId(ipsid).deviceId) + '-' +
            IntToStr(TPackedId(ipsid).dataSourceId) + '-' +
            IntToStr(TPackedId(ipsid).agentId) + '-' +
            IntToStr(TPackedId(ipsid).clusterId);
  if TPackedId(ipsid).clusterid<>123 then
    halt(1);
  if TPackedId(ipsid).agentid<>45678 then
    halt(2);
  if TPackedId(ipsid).datasourceid<>9012 then
    halt(3);
  if TPackedId(ipsid).deviceid<>34567 then
    halt(4);
  if TPackedId(ipsid).esmid<>89 then
    halt(5);

end;

var
  pi: TPackedId;
begin
  pi.clusterid:=123;
  pi.agentid:=45678;
  pi.datasourceid:=9012;
  pi.deviceid:=34567;
  pi.esmid:=89;
  writeln(PackedIdToStr(qword(pi)));
end.
