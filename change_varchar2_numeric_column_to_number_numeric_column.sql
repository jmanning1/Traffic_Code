--Change varchar2 numeric column to number numeric column 
select 
  'select "'||COLUMN_NAME||'" from '||TABLE_NAME||';
    update '||TABLE_NAME||' set "'||COLUMN_NAME||'" = regexp_replace("'||COLUMN_NAME||'", ''[^0-9|-]'', '''');
    update '||TABLE_NAME||' set "'||COLUMN_NAME||'" = null where "'||COLUMN_NAME||'" = ''-'';
    alter table '||TABLE_NAME||' add "'||COLUMN_NAME||'n" number;
    update '||TABLE_NAME||' set "'||COLUMN_NAME||'n" = "'||COLUMN_NAME||'";
    alter table '||TABLE_NAME||' drop column "'||COLUMN_NAME||'";
    alter table '||TABLE_NAME||' rename column "'||COLUMN_NAME||'n" to "'||COLUMN_NAME||'";'
from 
	user_tab_columns
where 
	data_type = 'VARCHAR2'
order by table_name, column_name
;

--Select column to be changed
select "_A_a_a__GSV_" from MAP_1_DATA_EXTRACT_GEOM;
--Remove all non-numeric characters/non-dash characters
update MAP_1_DATA_EXTRACT_GEOM set "_A_a_a__GSV_" = regexp_replace("_A_a_a__GSV_", '[^0-9|-]', '');
--Set value to null where only single '-' present
update MAP_1_DATA_EXTRACT_GEOM set "_A_a_a__GSV_" = null where "_A_a_a__GSV_" = '-';
--Add a new numeric column of the original varchar2 column
alter table MAP_1_DATA_EXTRACT_GEOM add "_A_a_a__GSV_n" number;
--Set new numeric column to equal original varchar2 column
update MAP_1_DATA_EXTRACT_GEOM set "_A_a_a__GSV_n" = "_A_a_a__GSV_";
--Drop original varchar2 column
alter table MAP_1_DATA_EXTRACT_GEOM drop column "_A_a_a__GSV_";
--Rename numeric column
alter table MAP_1_DATA_EXTRACT_GEOM rename column "_A_a_a__GSV_n" to "_A_a_a__GSV_";