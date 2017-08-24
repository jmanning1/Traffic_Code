--VIEW SDO_GEOM_METADATA
select * from user_sdo_geom_metadata order by table_name;
--VIEW Spatial Indexes
select table_name, index_name from user_indexes where ITYP_OWNER = 'MDSYS';



--UPDATE and CREATE Spatial Metadata and Spatial Indexes for all tables
--1) Delete USER_SDO_GEOM_METADATA
delete from USER_SDO_GEOM_METADATA;

--2) Write SQL to update SDO_GEOM_META_DATA
select 'insert into USER_SDO_GEOM_METADATA(TABLE_NAME, COLUMN_NAME, DIMINFO, SRID) values ('''||table_name||''', '''||column_name||''', mdsys.sdo_dim_array(MDSYS.SDO_DIM_ELEMENT(''X'',0,700000,0.005), MDSYS.SDO_DIM_ELEMENT(''Y'',0,1300000,0.005)), 27700);' from user_tab_columns where data_type = 'SDO_GEOMETRY';

--3) Write SQL to drop spatial indexes
select 'drop index '||index_name||';' from user_indexes where table_name in (select table_name from user_sdo_geom_metadata where ITYP_OWNER = 'MDSYS');

--4) Write SQL to create spatial indexes
	--Generic Spatial Indexes, Perferred Use
    
select 'create index '||table_name||'_SX on '||table_name||' ('||column_name||') indextype is mdsys.spatial_index;'from user_sdo_geom_metadata where table_name in (select table_name from user_tables);

--UPDATE Individual SDO_GEOM_METADATA Value & Spatial Indexes
--This can be used to used to update the spatial data for 1 table
delete from user_sdo_geom_metadata where table_name = 'WW_CHECK_AREA'; 
insert into USER_SDO_GEOM_METADATA 
		(TABLE_NAME, COLUMN_NAME, DIMINFO, SRID) 
		values 
			('WW_CHECK_AREA',
			'GEOM',
			mdsys.sdo_dim_array(
				MDSYS.SDO_DIM_ELEMENT('X',0,700000,0.005), 
				MDSYS.SDO_DIM_ELEMENT('Y',0,1300000,0.005)), 
			27700
			);
	
commit;
drop index Q_WW_CHECK_AREA;
create index Q_WW_CHECK_AREA on WW_CHECK_AREA(GEOM) indextype is mdsys.spatial_index;



--UPDATE all SDO_GEOM_METADATA values to match one existing SDO_GEOM_METADATA values
--Can be used to update all tables where there is 1 table with the correct SDO_GEOM_METADATA
update user_sdo_geom_metadata
set diminfo = (select diminfo from user_sdo_Geom_metadata where table_name='EL_CABLE' and column_name = 'GEOMETRY');


--Finding spatial tables without any metadata
select table_name, column_name
from user_tab_columns
where data_type = 'SDO_GEOMETRY'
and (table_name, column_name) not in (
select table_name, column_name from user_sdo_geom_metadata
)
order by table_name;

--Finding spatial tables without any spatial index
select table_name, column_name
from user_tab_columns
where data_type = 'SDO_GEOMETRY'
and (table_name, column_name) not in (
select table_name, column_name from user_sdo_index_info
)
order by table_name;

--Assign SRID to British National Grid
--Note: Need to Drop Spatial Index Before Hand
update table_name t set t.SHAPE.sdo_srid = 27700;

--Add Primary Key
--Note:  Change GID to PRIMARY KEY Field
Select 'ALTER TABLE '||table_name||' ADD CONSTRAINT '||table_name||'_PK PRIMARY KEY (GID);'from user_tables where table_name in (select table_name from user_tables);

--Drop Primary Key
--Note:  Change OBJECTID to PRIMARY KEY Field
Select 'ALTER TABLE '||table_name||' DROP CONSTRAINT '||table_name||'_PK;'from user_tables where table_name in (select table_name from user_tables) and table_name not like 'MD%' ;


--Convert from 3D to 2D:

  -- Drop Spatial Indexes

  --Update metadata to 2D

update user_sdo_geom_metadata set diminfo =
sdo_dim_array(sdo_dim_element('x', 140000, 380000, 0.0005), sdo_dim_element('y', 300000, 460000, 0.0005));

  --Remove Z Value

  select 'update '||table_name||' set '||column_name||' = sdo_cs.make_2d('||column_name||');
commit;' from user_tab_columns where data_type = 'SDO_GEOMETRY';

  --Recreate Spatial Indexes

--Convert from 2D to 3D:

  -- Drop Spatial Indexes

  --Update metadata to 3D

update user_sdo_geom_metadata set diminfo =
sdo_dim_array(sdo_dim_element('x', 140000, 380000, 0.0005), sdo_dim_element('y', 300000, 460000, 0.0005), sdo_dim_element('z', -1000, 1000, 0.0005));

  --Add Z Value

select 'update ' || utc.table_name || ' set ' || utc.column_name || ' = sdo_cs.make_3d(' || utc.column_name || ', 0) where '
|| column_name || ' is not null;'
from user_tables ut, user_tab_columns utc
where ut.table_name = utc.table_name
and utc.data_type = 'SDO_GEOMETRY';

  --Recreate Spatial Indexes