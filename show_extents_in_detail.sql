set linesize 120
column "Size (Mbytes)" format a15
column "Size (blocks)" format a15
column "Type" format a25
column "Tablespace" format a15
column "Table Name" format a20

SELECT SUBSTR(r.tab, 1, 20) "Table Name",
       SUBSTR(DECODE(GROUPING(r.type), 1, 'TOTAL', r.type), 1, 16) "Type",
       SUBSTR(r.tab_space, 1, 14) "Tablespace",
       SUBSTR(SUM(r.num_bytes)/1024, 1, 12) "Size (Mbytes)",
       SUBSTR(SUM(r.num_blocks), 1, 13) "Size (blocks)"
FROM
(
SELECT NVL(q.sp_idx_tab,
           NVL(q.sp_idx_tab_idx,
               NVL(q.sp_idx_tab_lob,
                   NVL(q.tab_lob,
                       NVL(q.tab_idx,
                           NVL(q.tab_iot_over, q.name)))))) tab,
       NVL2(q.sp_idx_tab, 'SPAT IDX TABLE',
            NVL2(q.sp_idx_tab_idx, 'SPAT IDX TAB IDX',
                 NVL2(q.sp_idx_tab_lob, 'SPAT IDX TAB LOB',
                      NVL2(q.tab_lob, 'TABLE LOB',
                           NVL2(q.tab_idx, (SELECT ui.index_type
                                              FROM user_indexes ui
                                             WHERE ui.index_name = q.name) ||
                                           ' INDEX',
                                NVL2(q.tab_iot_over, 'IOT OVERFLOW',
                                     'TABLE')))))) type,
       tab_space,
       num_bytes,
       num_blocks
  FROM
(
SELECT (SELECT ii.table_name FROM user_sdo_index_info ii
         WHERE ii.sdo_index_table = ue.segment_name) sp_idx_tab,
       (SELECT ii.table_name FROM user_indexes ui, user_sdo_index_info ii
         WHERE ui.index_name = ue.segment_name
           AND ii.sdo_index_table = ui.table_name) sp_idx_tab_idx,
       (SELECT ii.table_name FROM user_lobs ul, user_sdo_index_info ii
         WHERE ul.segment_name = ue.segment_name
           AND ii.sdo_index_table = ul.table_name) sp_idx_tab_lob,
       (SELECT ul.table_name FROM user_lobs ul
         WHERE ul.segment_name = ue.segment_name) tab_lob,
       (SELECT ui.table_name FROM user_indexes ui
         WHERE ui.index_name = ue.segment_name) tab_idx,
       (SELECT ut.iot_name FROM user_tables ut
         WHERE ut.table_name = ue.segment_name) tab_iot_over,
       ue.segment_name name,
       ue.tablespace_name tab_space,
       SUM(ue.bytes) num_bytes,
       SUM(ue.blocks) num_blocks
  FROM user_extents ue
 GROUP BY ue.segment_name, ue.segment_type, ue.tablespace_name
) q
) r
GROUP BY GROUPING SETS ( (r.tab, r.type, r.tab_space), (r.tab) )
ORDER BY r.tab, r.type;
