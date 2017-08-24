---
--- *** As the System user
---
---   show parameter case
---   NAME                                 TYPE        VALUE
------------------------------------------ ----------- -----------------
---   sec_case_sensitive_logon             boolean     TRUE
---
--- *** Set passwords to be non-case sensitive
---
   ALTER SYSTEM SET SEC_CASE_SENSITIVE_LOGON=FALSE SCOPE=BOTH;
   COMMIT;

---
--- *** Set the DEFAULT profile so that passwords do not expire
---   
   ALTER PROFILE DEFAULT LIMIT
     FAILED_LOGIN_ATTEMPTS UNLIMITED
     PASSWORD_LIFE_TIME UNLIMITED;
   COMMIT;

---
--- *** And also let's turn off the default auditing:
---
   NOAUDIT ALL;
   DELETE FROM SYS.AUD$;
   COMMIT;
 
---
--- *** Modify users to use the DEFAULT profile.
---
   ALTER USER RS_MARKUPS IDENTIFIED BY RS_MARKUPS PROFILE DEFAULT;
   ALTER USER RS_REPOSITORY IDENTIFIED BY RS_REPOSITORY PROFILE DEFAULT;
   ALTER USER RS_MASTERMAP IDENTIFIED BY RS_MASTERMAP PROFILE DEFAULT;
   ALTER USER RS_MIDCOAST IDENTIFIED BY RS_MIDCOAST PROFILE DEFAULT;
   ALTER USER RS_NETWORK IDENTIFIED BY RS_NETWORK PROFILE DEFAULT;
   ALTER USER RS_PHILLY IDENTIFIED BY RS_PHILLY PROFILE DEFAULT;
   ALTER USER RS_UTILIT IDENTIFIED BY RS_UTILIT PROFILE DEFAULT;
   COMMIT;
---
---
--- *** The end 