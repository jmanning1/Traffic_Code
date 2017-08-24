set feedback off
set trimspool off
spool results.csv
declare
	l_sql varchar2(4000);
	l_destination varchar(100);
	l_star_type varchar(100);
  l_type_winstar number;
	l_cnt	number;
	l_rc	sys_refcursor;
	l_found boolean;
begin

	-- Print CSV header
	dbms_output.put_line('Table Name,Destination,Star Type,Type Winstar,Count');

	-- Loop through the candidate BT* tables
	for tab_rec in (
		select table_name
		from user_tables
		where table_name like 'BT%BT%' 
		or table_name like 'BT%EP'
		or table_name like 'BT%HT2%'
		and table_name not like 'MD%'
		order by table_name
	)
	loop

		l_sql := '
		select a.destination, b.star_type, b.type_winstar, b.cnt
		from tecteo_tc_data.aerien a, (
			select 
				b.star_layer, 
				case 
					when star_type_lg is not null then ''Line''
					when star_type_pt is not null then ''Point''
					when star_type_tx is not null then ''Text''
					when star_type_su is not null then ''Null''
				end as star_type,
				max(nvl(b.star_type_lg,0) + nvl(b.star_type_pt,0) + nvl(b.star_type_su,0) + nvl(b.star_type_tx,0)) as type_winstar, 
				count(*) as cnt
			from ' || tab_rec.table_name || ' b 
			group by b.star_layer, star_type_lg, star_type_pt, star_type_su, star_type_tx) b
		where a.type_winstar = b.type_winstar
		and a.couche = b.star_layer';
		
		l_found	:= false;
		
		-- Open an explicit cursor for the sql above
    open l_rc for l_sql;
    loop 
      fetch l_rc into l_destination, l_star_type, l_type_winstar, l_cnt; 
      exit when l_rc%notfound;		
			
			l_found := true;
			-- Print the values out as CSV
			dbms_output.put_line(tab_rec.table_name || ',' || l_destination || ',' || l_star_type || ',' ||  l_type_winstar  || ',' || l_cnt);
    
		end loop l_rc;	
    close l_rc;	
			
		if not l_found then
			-- If no record was found, then print out a blank row
			dbms_output.put_line(tab_rec.table_name || ',' || null || ',' || null || ',' ||  null  || ',' || null);
		end if;
		
	end loop tab_rec;

end;
/
spool off