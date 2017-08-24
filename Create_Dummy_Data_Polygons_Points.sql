create table nearest_house_test (id number,geom sdo_geometry);

create table nearest_asset_test (id number,geom sdo_geometry);
-----

truncate table nearest_house_test;

truncate table nearest_asset_test;
 
insert into Nearest_House_test values (
1, 
SDO_GEOMETRY(	2003, 
		27700,	
		null,	
SDO_ELEM_INFO_ARRAY(	1,1003,1),
SDO_ORDINATE_ARRAY(
   375124     ,   409402   , 
   375053     ,   409423     , 
   375030     ,   409345    ,
   375053     ,   409339     , 
   375128     ,   409317     , 
   375180     ,   409302     ,  
   375184     ,   409317    ,  
   375150     ,   409327   , 
   375132     ,   409332    ,  
   375114     ,   409337    ,  
   375132     ,   409400    ,  
   375124     ,   409402    ))
);

insert into Nearest_House_test values (
2, 
SDO_GEOMETRY(	2003, 
		27700,	
		null,	
SDO_ELEM_INFO_ARRAY(	1,1003,1),
SDO_ORDINATE_ARRAY(
   375424     ,   409402   , 
   375353     ,   409423     , 
   375330     ,   409345    ,
   375353     ,   409339     , 
   375428     ,   409317     , 
   375480     ,   409302     ,  
   375484     ,   409317    ,  
   375450     ,   409327   , 
   375432     ,   409332    ,  
   375414     ,   409337    ,  
   375432     ,   409400    ,  
   375424     ,   409402    ))
);

insert into Nearest_House_test values (
3, 
SDO_GEOMETRY(	2003, 
		27700,	
		null,	
SDO_ELEM_INFO_ARRAY(	1,1003,1),
SDO_ORDINATE_ARRAY(
   375124     ,   409702   , 
   375053     ,   409723     , 
   375030     ,   409645    ,
   375053     ,   409639     , 
   375128     ,   409617     , 
   375180     ,   409602     ,  
   375184     ,   409617    ,  
   375150     ,   409627   , 
   375132     ,   409632    ,  
   375114     ,   409637    ,  
   375132     ,   409700    ,  
   375124     ,   409702    ))
);

insert into Nearest_House_test values (
4, 
SDO_GEOMETRY(	2003, 
		27700,	
		null,	
SDO_ELEM_INFO_ARRAY(	1,1003,1),
SDO_ORDINATE_ARRAY(
   375424     ,   409702   , 
   375353     ,   409723     , 
   375330     ,   409645    ,
   375353     ,   409639     , 
   375428     ,   409617     , 
   375480     ,   409602     ,  
   375484     ,   409617    ,  
   375450     ,   409627   , 
   375432     ,   409632    ,  
   375414     ,   409637    ,  
   375432     ,   409700    ,  
   375424     ,   409702    ))
);


insert into Nearest_Asset_test values (
1, 
SDO_GEOMETRY(	2001, 	
		27700,	
		SDO_POINT_TYPE(375200,409500,null), 	
null,
null)
);

insert into Nearest_Asset_test values (
2, 
SDO_GEOMETRY(	2001, 	
		27700,	
		SDO_POINT_TYPE(375310,409500,null), 	
null,
null)
);

insert into Nearest_Asset_test values (
3, 
SDO_GEOMETRY(	2001, 	
		27700,	
		SDO_POINT_TYPE(375200,409510,null), 	
null,
null)
);

insert into Nearest_Asset_test values (
4, 
SDO_GEOMETRY(	2001, 	
		27700,	
		SDO_POINT_TYPE(375310,409510,null), 	
null,
null)
);

commit;
