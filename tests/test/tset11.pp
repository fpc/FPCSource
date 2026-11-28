{ typinfo with sets with elements >255 and sets with a set base <>0 }
{$mode objfpc}
program tset11;
uses
  sysutils, typinfo;

type
  tbig = set of 0..1000;
  tbased = set of 1000..1010;
  tenum = (
    e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14,
    e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27, e28, e29,
    e30, e31, e32, e33, e34, e35, e36, e37, e38, e39, e40, e41, e42, e43, e44,
    e45, e46, e47, e48, e49, e50, e51, e52, e53, e54, e55, e56, e57, e58, e59,
    e60, e61, e62, e63, e64, e65, e66, e67, e68, e69, e70, e71, e72, e73, e74,
    e75, e76, e77, e78, e79, e80, e81, e82, e83, e84, e85, e86, e87, e88, e89,
    e90, e91, e92, e93, e94, e95, e96, e97, e98, e99, e100, e101, e102, e103, e104,
    e105, e106, e107, e108, e109, e110, e111, e112, e113, e114, e115, e116, e117, e118, e119,
    e120, e121, e122, e123, e124, e125, e126, e127, e128, e129, e130, e131, e132, e133, e134,
    e135, e136, e137, e138, e139, e140, e141, e142, e143, e144, e145, e146, e147, e148, e149,
    e150, e151, e152, e153, e154, e155, e156, e157, e158, e159, e160, e161, e162, e163, e164,
    e165, e166, e167, e168, e169, e170, e171, e172, e173, e174, e175, e176, e177, e178, e179,
    e180, e181, e182, e183, e184, e185, e186, e187, e188, e189, e190, e191, e192, e193, e194,
    e195, e196, e197, e198, e199, e200, e201, e202, e203, e204, e205, e206, e207, e208, e209,
    e210, e211, e212, e213, e214, e215, e216, e217, e218, e219, e220, e221, e222, e223, e224,
    e225, e226, e227, e228, e229, e230, e231, e232, e233, e234, e235, e236, e237, e238, e239,
    e240, e241, e242, e243, e244, e245, e246, e247, e248, e249, e250, e251, e252, e253, e254,
    e255, e256, e257, e258, e259, e260, e261, e262, e263, e264, e265, e266, e267, e268, e269,
    e270, e271, e272, e273, e274, e275, e276, e277, e278, e279, e280, e281, e282, e283, e284,
    e285, e286, e287, e288, e289, e290, e291, e292, e293, e294, e295, e296, e297, e298, e299);
  tenumset = set of tenum;
  tsmallenum = (s0,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10);
{$packset 1}
  tsubset = set of s8..s10;
{$packset default}

procedure error(nr: longint);
  begin
    writeln('error ',nr);
    halt(nr);
  end;

{ checks that p raises an EPropertyError }
type
  tproc = procedure;

procedure expect_error(p: tproc; nr: longint);
  begin
    try
      p();
    except
      on EPropertyError do
        exit;
    end;
    error(nr);
  end;

var
  big: tbig;
  based: tbased;
  es: tenumset;
  sub: tsubset;
  a: TBytes;

procedure based_to_array;
  begin
    SetToArray(PTypeInfo(TypeInfo(tbased)),@based);
  end;

procedure big_out_of_range;
  begin
    StringToSet(PTypeInfo(TypeInfo(tbig)),'1001',@big);
  end;

procedure sub_array_out_of_range;
  begin
    ArrayToSet(PTypeInfo(TypeInfo(tsubset)),[7],@sub);
  end;

begin
  if sizeof(tbased)<>2 then
    error(1);
  if sizeof(tsubset)<>1 then
    error(2);

  { integer elements >255 }
  big:=[3,300,999,1000];
  if SetToString(PTypeInfo(TypeInfo(tbig)),@big,true)<>'[3,300,999,1000]' then
    error(10);
  big:=[];
  StringToSet(PTypeInfo(TypeInfo(tbig)),'[1000,0,256]',@big);
  if big<>[0,256,1000] then
    error(11);
  expect_error(@big_out_of_range,12);

  { set base 1000 }
  based:=[1000,1005,1010];
  if SetToString(PTypeInfo(TypeInfo(tbased)),@based)<>'1000,1005,1010' then
    error(20);
  based:=[];
  StringToSet(PTypeInfo(TypeInfo(tbased)),'1007',@based);
  if based<>[1007] then
    error(21);
  { elements >255 do not fit into TBytes }
  expect_error(@based_to_array,22);

  { enum with more than 256 values }
  es:=[e5,e256,e299];
  if SetToString(PTypeInfo(TypeInfo(tenumset)),@es)<>'e5,e256,e299' then
    error(30);
  es:=[];
  StringToSet(PTypeInfo(TypeInfo(tenumset)),'e299,e1',@es);
  if es<>[e1,e299] then
    error(31);

  { set base 8 with packset 1 }
  sub:=[s9];
  if SetToString(PTypeInfo(TypeInfo(tsubset)),@sub)<>'s9' then
    error(40);
  StringToSet(PTypeInfo(TypeInfo(tsubset)),'s8,s10',@sub);
  if sub<>[s8,s10] then
    error(41);
  a:=SetToArray(PTypeInfo(TypeInfo(tsubset)),@sub);
  if (length(a)<>2) or (a[0]<>8) or (a[1]<>10) then
    error(42);
  ArrayToSet(PTypeInfo(TypeInfo(tsubset)),[9],@sub);
  if sub<>[s9] then
    error(43);
  expect_error(@sub_array_out_of_range,44);

  writeln('ok');
end.
