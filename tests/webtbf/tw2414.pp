{ %fail }

USES uw2414;

VAR
        ob      : anObj;
BEGIN
        ob.k:=8;
        ob.A('xxyyzzww');
        ob.A('aabbcc',4);  { call to a private method allowed !? }
END
.
