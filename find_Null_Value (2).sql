 set serveroutput on
 begin
	declare 
		tab varchar2(1000);
		col varchar2(1000);
		cnt number;
		findstr varchar2(2000) :='XXX';
 begin
	for recs in (select 'SELECT '''||table_name||''' tab,'''||column_name||''' col, count(1) cnt from '||table_name||' t where  t.'||column_name||' =  '''||findstr||''' ' sql_code
				from user_tab_columns t
				where DATA_TYPE = 'VARCHAR2' and data_length >= length(findstr)
				order by 1) Loop
		begin
			execute immediate (recs.sql_code) into tab,col,cnt ;
			
			--dbms_output.put_line (recs.sql_code);
			if cnt > 0 then
				dbms_output.put_line (tab || '.'||col||' has '||cnt||' instences of '||findstr);
			end if;
			
		end;				
	end loop;
end;
end;
/