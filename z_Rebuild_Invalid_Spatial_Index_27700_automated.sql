prompt .  
prompt ===================================================== 
prompt .                                                   .
prompt .  Get everything indexed - assume srid 27700 
prompt .                                                   .
prompt ===================================================== 
prompt .  Version 1.0 | Alan Howie | 1Spatial | FEB 2015   .  
prompt =====================================================
prompt . Steps :- 
prompt .  
prompt =====================================================


-- some initial session settings 
set serveroutput on 
set time on
set timi on 

-- command to drop invalid spatial indexes 
begin
	for recs in (SELECT 'DROP INDEX '||INDEX_NAME sql_code
				 from user_indexes   
				 where ityp_name = 'SPATIAL_INDEX' 
				 and DOMIDX_OPSTATUS = 'FAILED') Loop
		begin			
			execute immediate (recs.sql_code); 
			dbms_output.put_line (recs.sql_code||' (OK)');
		exception
			when others then
				dbms_output.put_line (recs.sql_code||' (FAILED)');
		end;	
		commit;
	end loop;
end;
/

-- command to create missing metadata srid 27700
begin
	for recs in (select distinct 'insert into user_sdo_geom_metadata values ('''||table_name||''','''||column_name||''','||
	                              'MDSYS.SDO_DIM_ARRAY(MDSYS.SDO_DIM_ELEMENT(''X'',0,700000,0.001),'||
								                      'MDSYS.SDO_DIM_ELEMENT(''Y'',0,1300000,0.001)),27700)' sql_code
				from user_tab_columns t
				where DATA_TYPE = 'SDO_GEOMETRY'
				and table_name not in (select distinct table_name from user_Sdo_geom_metadata)
				order by 1) Loop
		begin			
			execute immediate (recs.sql_code); 
			dbms_output.put_line (recs.sql_code||' (OK)');
		exception
			when others then
				dbms_output.put_line (recs.sql_code||' (FAILED)');
		end;	
		commit;
	end loop;
end;
/

-- create spatial indexes 
begin
	for recs in (select distinct 'CREATE INDEX '||substr(table_name||'_'||column_name,1,25)||'_SPID ON '||table_name||'('||column_name||')   INDEXTYPE IS MDSYS.SPATIAL_INDEX' sql_code
				from user_tab_columns t
				where DATA_TYPE = 'SDO_GEOMETRY'
				and table_name in (select distinct object_name from user_objects where object_type = 'TABLE')
				and table_name not in (select distinct table_name from user_indexes where ityp_name = 'SPATIAL_INDEX')
				order by 1) Loop
		begin			
			execute immediate (recs.sql_code); 
			dbms_output.put_line (recs.sql_code||' (OK)');
		exception
			when others then
				dbms_output.put_line (recs.sql_code||' (FAILED)');
		end;	
		commit;
	end loop;
end;
/

