program fpctest4;
{$ifdef fpc}
{$mode delphi}
{$endif fpc}
uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif}
  Classes,sysutils,variants,typinfo;

type
   TTestClass=class(TComponent)
   private
     fV1,fV2,fV3:variant;
     procedure SetV2(const v:variant);
   published
     property V1:Variant read fV1 write fV1;
     property V2:Variant read fV2 write SetV2;
     property V3:Variant read fV3 write fV3;
   end;
	
const
   {$ifdef fpc}
   ws:WideString=#$43f#$440#$438#$432#$435#$442', '#$43f#$440#$44B#$432#$456#$442#$430#$43d#$44c#$43d#$435' - pr'#$fc'fung spa'#$df' gut';
   {$else}
   ws:WideString='ÔË‚ÂÚ, Ô˚‚≥Ú‡Ì¸ÌÂ - prufung spa'#$df' gut';
   {$endif}

procedure TTestClass.SetV2(const v:variant);
begin
  fV2:=v;
  writeln('Set V2');
end;

var tc:TTestClass;
    f:TStream;
    vv:variant;
    ff : TFileStream;
begin

   RegisterClasses([TTestClass]);


   tc:=TTestClass.Create(nil);
   tc.v1:=123.45;
   tc.v2:='Hello world';
   tc.v3:=ws;

   vv:=GetVariantProp(tc,'V2');
   if vv<>Null then
     begin
       if (vv<>'Hello world') then
         halt(1);
       writeln('got=',vv);
     end
   else
     halt(2);


   SetVariantProp(tc,'V1',333.333);

   vv:=GetVariantProp(tc,'V1');
   if vv<>Null then
     begin
       if (vv<>333.333) then
         halt(3);
       writeln('got=',vv);
     end
   else
     halt(4);

   f:=TMemoryStream.Create;
   f.WriteComponent(tc); // store it

   ff:=TFileStream.Create('tw19482.str',fmCreate);
   ff.WriteComponent(tc); // store it
   ff.Free;

   tc.Free; // kill it

   f.free;

   f:=TFileStream.Create('tw19482.str',fmOpenRead);

   tc:=TTestClass(f.ReadComponent(nil));

   writeln('v1=',tc.v1);
   writeln('v2=',tc.v2);
   writeln('v3=',tc.v3);
   if (tc.v1<>333.333) then
     halt(5);
   if (tc.v2<>'Hello world') then
     halt(6);
   if (tc.v3<>ws) then
     halt(7);

   f.Free;

   DeleteFile('tw19482.str')
end.
