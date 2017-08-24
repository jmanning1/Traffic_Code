
create or replace function get_max_xy_precision(geometry in sdo_geometry) 
return number
is
	l_max_precision number := 0;
	l_ords sdo_ordinate_array;
	l_x_precision number;
	l_y_precision number;	
begin

	if geometry is null or geometry.sdo_ordinates is null then
		return null;
	else
		dbms_output.put_line('sdo_ordinates is not null');
		l_ords := geometry.sdo_ordinates;
		for i in 1..(l_ords.count/3)
		loop
			dbms_output.put_line('i ' || i || ' dx ' || l_ords((i*3)-2) || ' dy ' || l_ords((i*3)-1) || ' dz ' || l_ords((i*3)));
			l_x_precision := length(l_ords((i*3)-2) - floor(l_ords((i*3)-2)))-1;			
			l_y_precision := length(l_ords((i*3)-1) - floor(l_ords((i*3)-1)))-1;			
			
			if l_x_precision > l_max_precision then
				dbms_output.put_line('l_x_precision ' || l_x_precision || ' > l_max_precision ' || l_max_precision);
				l_max_precision := l_x_precision;
			end if;
			if l_y_precision > l_max_precision then
				dbms_output.put_line('l_y_precision ' || l_y_precision || ' > l_max_precision ' || l_max_precision);			
				l_max_precision := l_y_precision;
			end if;
			
		end loop i;
		
		return l_max_precision;
	end if;
	
end;
/
show errors


--Select Statement to call Function:

select get_max_xy_precision(geom) as max_precision, count(*)
from  TR_TRANSPORT_CASING 
group by get_max_xy_precision(geom)
order by 1;

select 'select get_max_xy_precision(geom) as max_precision, count(*)
from  '||table_name||' 
 group by get_max_xy_precision(geom)
order by 1;' from user_tables where table_name in (select table_name from user_tables);