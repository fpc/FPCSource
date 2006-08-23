{$bitpacking on}

{ from the GPC test suite (sam9.pas) }

program sam9;

type
  e1 = (
    enum000,
    enum001,
    enum002,
    enum003,
    enum004,
    enum005,
    enum006,
    enum007,
    enum008,
    enum009,
    enum010,
    enum011,
    enum012,
    enum013,
    enum014,
    enum015,
    enum016,
    enum017,
    enum018,
    enum019,
    enum020,
    enum021,
    enum022,
    enum023,
    enum024,
    enum025,
    enum026,
    enum027,
    enum028,
    enum029,
    enum030,
    enum031,
    enum032,
    enum033,
    enum034,
    enum035,
    enum036,
    enum037,
    enum038,
    enum039,
    enum040,
    enum041,
    enum042,
    enum043,
    enum044,
    enum045,
    enum046,
    enum047,
    enum048,
    enum049,
    enum050,
    enum051,
    enum052,
    enum053,
    enum054,
    enum055,
    enum056,
    enum057,
    enum058,
    enum059,
    enum060,
    enum061,
    enum062,
    enum063,
    enum064,
    enum065,
    enum066,
    enum067,
    enum068,
    enum069,
    enum070,
    enum071,
    enum072,
    enum073,
    enum074,
    enum075,
    enum076,
    enum077,
    enum078,
    enum079,
    enum080,
    enum081,
    enum082,
    enum083,
    enum084,
    enum085,
    enum086,
    enum087,
    enum088,
    enum089,
    enum090,
    enum091,
    enum092,
    enum093,
    enum094,
    enum095,
    enum096,
    enum097,
    enum098,
    enum099,
    enum100,
    enum101,
    enum102,
    enum103,
    enum104,
    enum105,
    enum106,
    enum107,
    enum108,
    enum109,
    enum110,
    enum111,
    enum112,
    enum113,
    enum114,
    enum115,
    enum116,
    enum117,
    enum118,
    enum119,
    enum120,
    enum121,
    enum122,
    enum123,
    enum124,
    enum125,
    enum126,
    enum127,
    enum128 { Remove this and it works !}
  );

  r1 = 0 .. 128;

  t1 = packed record { has to be packed }
    case integer of
        1: (f1: e1);
        2: (f2: r1);
      end;

var
  v1: t1;

procedure foo;
begin
  v1.f1 := enum000;
  v1.f2 := 127;
  v1.f2 := 128;
end;

begin
  foo;
  if v1.f1 <> enum128 then
    halt(1);
end.
