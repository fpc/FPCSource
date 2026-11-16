-- Migration script: v1 -> v2
-- Generated: 2026-04-19 23:07:56
-- Database: gen_migrate_demo.db

CREATE TABLE CustOrder (ID INT, CustomerID INT, Product VARCHAR(255), Amount FLOAT, CONSTRAINT CustOrder_PK PRIMARY KEY(ID));
ALTER TABLE Customer ADD COLUMN Email VARCHAR(255);
ALTER TABLE Customer ADD COLUMN Country VARCHAR(255);
INSERT INTO _opf_migrations (Version, AppliedAt, Description) VALUES (2, '2026-04-19 23:07:56', 'Migration to version 2');
