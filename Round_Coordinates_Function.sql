create or replace function round_coordinates(p_geometry in sdo_geometry, p_round_factor in number)
return sdo_geometry
is
 l_dim         	pls_integer;
 l_gtype       	pls_integer;
 l_geometry    	sdo_geometry := p_geometry;
 l_ordinates   	sdo_ordinate_array;
begin

	l_gtype := mod(p_geometry.sdo_gtype, 10);
	l_dim 	:= substr(p_geometry.sdo_gtype,1,1);
	l_ordinates := p_geometry.sdo_ordinates;		  

	-- if point update differently to other shapes...
	if ( l_geometry.sdo_point is not null ) then
		l_geometry.sdo_point.x := round(l_geometry.sdo_point.x, p_round_factor);
		l_geometry.sdo_point.y := round(l_geometry.sdo_point.y, p_round_factor);
		if l_dim > 2 then
			l_geometry.sdo_point.z := round(l_geometry.sdo_point.z, p_round_factor);
		end if;
	end if;

	if ( l_gtype <> 1 ) then
		for i in l_ordinates.first..l_ordinates.last loop
			l_ordinates(i) := round(l_ordinates(i), p_round_factor);
		end loop;
	end if;

	return sdo_geometry(l_geometry.sdo_gtype,
		l_geometry.sdo_srid,
		l_geometry.sdo_point,
		l_geometry.sdo_elem_info,
		l_ordinates);
		
end round_coordinates;
/

-- TO apply Function
select 'update '||table_name||' set geom = round_coordinates(geom, 3) where geom is not null;' from user_tables where table_name in (select table_name from user_tables) and table_name not like 'MD%';

--To Only process those above the desired number of Characters
select 'update '||table_name||' set geom = round_coordinates(geom, 3) where get_max_xy_precision(geom) > 3 and geom is not null;' from user_tables where table_name in (select table_name from user_tables) and table_name not like 'MD%';