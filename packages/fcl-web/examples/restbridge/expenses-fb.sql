create domain bool smallint check (value in (1,0,Null));

create table ExpenseTypes (
  etID bigint not null,
  etName varchar(50) not null,
  etDescription varchar(100) not null,
  etMaxAmount decimal(10,2),
  etCost decimal(10,2) default 1,
  etActive bool default 1 not null
);


create table Users (
  uID bigint not null,
  uLogin varchar(50) not null,
  uFullName varchar(100) not null,
  uPassword varchar(100) not null,
  uActive bool default 1 not null
);

create table Projects (
  pID bigint not null,
  pName varchar(50) not null,
  pDescription varchar(100) not null,
  pActive bool default 1 not null
);

create table Expenses (
  eID bigint not null,
  eUserFK bigint not null,
  eProjectFK bigint not null,
  eTypeFK bigint not null,
  eAmount decimal(10,2) not null,
  eDate date default 'today' not null,
  eComment varchar(1024)
);

create sequence seqExpenseTypesID;
create sequence seqUsersID;
create sequence seqProjectsID;
create sequence seqExpenseID;

alter table ExpenseTypes add constraint pkExpenseTypes primary key (etID);
alter table Users add constraint pkUsers primary key (uID);
alter table Projects add constraint pkProjects primary key (pID);
alter table Expenses add  constraint pkExpenses primary key (eID);

SET TERM ^ ;
CREATE TRIGGER ExpenseTypesID FOR ExpenseTypes ACTIVE
BEFORE INSERT POSITION 0
AS
begin
  if (NEW.etID is null)  then
    NEW.etID=GEN_ID(seqExpenseTypesID,1);
end^

CREATE TRIGGER ExpensesID FOR Expenses ACTIVE
BEFORE INSERT POSITION 0
AS
begin
  if (NEW.eID is null)  then
    NEW.eID=GEN_ID(seqExpenseID,1);
end^

CREATE TRIGGER ProjectsID FOR Projects ACTIVE
BEFORE INSERT POSITION 0
AS
begin
  if (NEW.pID is null)  then
    NEW.pID=GEN_ID(seqProjectsID,1);
end^

CREATE TRIGGER UsersID FOR Users ACTIVE
BEFORE INSERT POSITION 0
AS
begin
  if (NEW.uID is null)  then
    NEW.uID=GEN_ID(seqUsersID,1);
end^

set term ^ ;

COMMIT ;



