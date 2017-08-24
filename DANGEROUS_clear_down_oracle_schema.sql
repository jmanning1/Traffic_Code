select * from cat;
begin
 
for c in (select * from user_objects) loop
begin
    if c.object_type = 'VIEW' then
        execute immediate 'drop view ' || c.object_name;
    elsif c.object_type = 'TABLE' then
        execute immediate 'drop table ' || c.object_name || ' cascade constraints';
    elsif c.object_type = 'SEQUENCE' then
        execute immediate 'drop sequence ' || c.object_name;
    elsif c.object_type = 'PACKAGE' then
        execute immediate 'drop package ' || c.object_name;
    elsif c.object_type = 'TRIGGER' then
        execute immediate 'drop trigger ' || c.object_name;
    elsif c.object_type = 'PROCEDURE' then
        execute immediate 'drop procedure ' || c.object_name;
    elsif c.object_type = 'FUNCTION' then
        execute immediate 'drop function ' || c.object_name;
    elsif c.object_type = 'SYNONYM' then
    execute immediate 'drop synonym ' || c.object_name;
        elsif c.object_type = 'INDEX' then
        execute immediate 'drop index ' || c.object_name;
    elsif c.object_type = 'PACKAGE BODY' then
        execute immediate 'drop package body ' || c.object_name;
    elsif c.object_type = 'DATABASE LINK' then
        execute immediate 'drop database link ' || c.object_name;
    end if;
    exception
    when others then
        null;
    end;
end loop;
end;
/
purge recyclebin;
select * from cat;
