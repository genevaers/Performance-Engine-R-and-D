//NCBDEMD4 JOB (ACCT),'GENEVAERS DEMO PASS4',                                   
//          NOTIFY=&SYSUID.,                                                    
//          REGION=0M,                                                          
//          CLASS=A,                                                            
//          MSGLEVEL=(1,1),                                                     
//          MSGCLASS=X                                                          
//*                                                                             
//         EXPORT SYMLIST=*                                                     
//*                                                                             
//*        SET HLQ=<YOUR-TSO-PREFIX>                                            
//         SET MLQ=GVBDEMO                                                      
//*                                                                             
//JOBLIB   DD DISP=SHR,DSN=&HLQ..MLQ..GVBLOAD                                   
//*                                                                             
//*********************************************************************         
//*                                                                             
//*     GVBDEME  - EXECUTE CONTROL FILE CREATION, REFERENCE AND EXTRACT         
//*                PHASES.                                                      
//*                                                                             
//* BEFORE SUBMITTING THIS JOB, PLEASE:                                         
//*                                                                             
//*     1)  UPDATE THE JOB STATEMENT ABOVE TO CONFORM TO YOUR                   
//*         INSTALLATION'S STANDARDS.                                           
//*                                                                             
//*     2)  SET THE VALUE OF "HLQ" ABOVE TO YOUR TSO PREFIX.                    
//*         THIS IS NORMALLY THE SAME AS YOUR TSO ID,                           
//*         UNLESS YOU HAVE CHANGED IT WITH THE TSO PROFILE PREFIX              
//*         COMMAND.                                                            
//*                                                                             
//*         THIS VALUE WILL DETERMINE THE HIGH-LEVEL QUALIFIER                  
//*         OF THE NAMES OF THE DEMO DATA SETS.                                 
//*                                                                             
//*     3)  THE "MLQ" DEFAULT VALUE IS GVBDEMO AND DOES NOT NEED TO BE          
//*         CHANGED.                                                            
//*                                                                             
//*********************************************************************         
//*                                                                             
//*********************************************************************         
//* DELETE THE FILE(S) CREATED IN NEXT STEP                                     
//*********************************************************************         
//*                                                                             
//PSTEP200 EXEC PGM=IDCAMS                                                      
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
//SYSIN    DD *,SYMBOLS=EXECSYS                                                 
                                                                                
 DELETE  &HLQ..&MLQ..PASS1C1.VDP.XML PURGE                                      
 DELETE  &HLQ..&MLQ..PASS1C1.VDP PURGE                                          
 DELETE  &HLQ..&MLQ..PASS1C1.JLT PURGE                                          
 DELETE  &HLQ..&MLQ..PASS1C1.XLT PURGE                                          
                                                                                
 IF MAXCC LE 8 THEN         /* IF OPERATION FAILED,     */    -                 
     SET MAXCC = 0          /* PROCEED AS NORMAL ANYWAY */                      
                                                                                
//*                                                                             
//*********************************************************************         
//* CREATE THE RUN CONTROL FILES                                                
//*********************************************************************         
//*                                                                             
//PSTEP205 EXEC PGM=GVBMR91                                                     
//STEPLIB  DD DISP=SHR,DSN=&HLQ..LATEST.GVBLOAD                                 
//*                                                                             
//DSNAOINI DD *                                                                 
[COMMON]                                                                        
MVSDEFAULTSSID=DM12                                                             
APPLTRACEFILENAME="DD:ODBCTRAC"                                                 
APPLTRACE=0                                                                     
                                                                                
[DM12]                                                                          
CURRENTSQLID=SAFRWBTR                                                           
UNDERSCORE=0                                                                    
                                                                                
//*                                                                             
//MR91PARM DD *                                                                 
 INPUT_TYPE               = WBXML      DEFAULT: (NONE)  DD: WBXMLI              
*INPUT_TYPE               = DB2        DEFAULT: (NONE)  DD: (DATABASE)          
 DB2_SUBSYSTEM            = DM12      DEFAULT: (NONE)                           
 DB2_ENVIRONMENT_ID       = 04        DEFAULT: (NONE)                           
 DB2_SCHEMA               = SAFRWBTR  DEFAULT: (NONE)                           
                                                                                
*OUTPUT_WBXML_FILES       = Y          DEFAULT: N       DD: WBXML               
*OUT_VDP_XML.ACTIVE       = Y          DEFAULT: N       DD: VDPXML              
 OUTPUT_RUN_CONTROL_FILES = Y          DEFAULT: N       DD: VDP,JLT,XLT         
                                                                                
*TRACE                    = Y           DEFAULT: N                              
*NUMBER_MODE              = LARGE      DEFAULT: STANDARD                        
//*                                                                 %%%         
//DBVIEWS  DD *                                                                 
//*                                                                             
//DBFLDRS  DD *                                                                 
//*                                                                             
//RUNVIEWS DD *                                                                 
10723                                                                           
//*                                                                             
//WBXMLI   DD DSN=NBEESLE.DEMO.OPEN.SOURCE.EXIT.VIEWS.XML,                      
//            DISP=SHR                                                          
//*                                                                             
//*        <<< OUTPUT GENEVAERS CONTROL FILES >>>                               
//*                                                                             
//VDPXML   DD DSN=&HLQ..&MLQ..PASS1C1.VDP.XML,                                  
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=SYSDA,                                                       
//            SPACE=(TRK,(1000,1000),RLSE),                                     
//            DSORG=PS,RECFM=VB,LRECL=32756                                     
//*                                                                             
//VDP      DD DSN=&HLQ..&MLQ..PASS1C1.VDP,                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=SYSDA,                                                       
//            SPACE=(TRK,(1000,1000),RLSE),                                     
//            DSORG=PS,RECFM=VB,LRECL=32756                                     
//*                                                                             
//JLT      DD DSN=&HLQ..&MLQ..PASS1C1.JLT,                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=SYSDA,                                                       
//            SPACE=(TRK,(1000,1000),RLSE),                                     
//            DSORG=PS,RECFM=VB,LRECL=820                                       
//*                                                                             
//XLT      DD DSN=&HLQ..&MLQ..PASS1C1.XLT,                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=SYSDA,                                                       
//            SPACE=(TRK,(1000,1000),RLSE),                                     
//            DSORG=PS,RECFM=VB,LRECL=820                                       
//*                                                                             
//CEEDUMP  DD SYSOUT=*                                                          
//LEMSG    DD SYSOUT=*                                                          
//IDIREPRT DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//MR91RPT  DD SYSOUT=*                                                          
//MR91LOG  DD SYSOUT=*                                                          
//MR91TRAC DD SYSOUT=*                                                          
//*                                                                             
//*********************************************************************         
//* IF THE PRIOR STEP SETS AN INVALID RETURN CODE, TERMINATE THE JOB            
//*********************************************************************         
//*                                                                             
//PSTEP206 EXEC PGM=GVBUT99,                                                    
//            COND=(4,GE,PSTEP205),                                             
//            PARM='1099'                                                       
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
//*********************************************************************         
//* DELETE THE FILE(S) CREATED IN NEXT STEP                                     
//*********************************************************************         
//*                                                                             
//PSTEP500 EXEC PGM=IDCAMS                                                      
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
//SYSIN    DD *,SYMBOLS=EXECSYS                                                 
                                                                                
 DELETE  &HLQ..&MLQ..PASS1D1.FILE001.RED PURGE                                  
 DELETE  &HLQ..&MLQ..PASS1D1.FILE002.RED PURGE                                  
 DELETE  &HLQ..&MLQ..PASS1D1.FILE003.RED PURGE                                  
 DELETE  &HLQ..&MLQ..PASS1D1.FILE004.RED PURGE                                  
 DELETE  &HLQ..&MLQ..PASS1D1.FILE005.RED PURGE                                  
 DELETE  &HLQ..&MLQ..PASS1D1.FILE006.RED PURGE                                  
 DELETE  &HLQ..&MLQ..PASS1D1.REH PURGE                                          
 DELETE  &HLQ..&MLQ..PASS1D1.RTH PURGE                                          
 DELETE  &HLQ..&MLQ..PASS1D1.SYSMDUMP PURGE                                     
                                                                                
 IF LASTCC > 0 THEN        /* IF OPERATION FAILED,     */    -                  
     SET MAXCC = 0          /* PROCEED AS NORMAL ANYWAY */                      
                                                                                
//*                                                                             
//*********************************************************************         
//*   PREPARE REFERENCE DATA                                                    
//*********************************************************************         
//*                                                                             
//PSTEP505 EXEC PGM=GVBMR95R,                                                   
// REGION=0M                                                                    
//*                                                                             
//*        <<< INPUT GENEVAERS FILES >>>                                        
//*                                                                             
//REFRPARM DD *                                                                 
*                                                                               
*   STANDARD OPTIONS                                                            
*-------------------                                                            
*                                                                               
*DISK_THREAD_LIMIT=10                   DEFAULT: 9999                           
*TAPE_THREAD_LIMIT=10                   DEFAULT: 9999                           
*                                                                               
*IO_BUFFER_LEVEL=8                      DEFAULT: 4                              
*OPTIMIZE_PACKED_OUTPUT=N               DEFAULT: Y                              
*PAGE_FIX_IO_BUFFERS=N                  DEFAULT: Y                              
*TREAT_MISSING_VIEW_OUTPUTS_AS_DUMMY=Y  DEFAULT: N                              
*ABEND_ON_CALCULATION_OVERFLOW=N        DEFAULT: Y                              
*                                                                               
*   DEBUGGING OPTIONS                                                           
*--------------------                                                           
*                                                                               
*TRACE=Y                                DEFAULT: N                              
*DUMP_LT_AND_GENERATED_CODE=Y           DEFAULT: N                              
*SOURCE_RECORD_LIMIT=100                DEFAULT: (NO LIMIT)                     
*ABEND_ON_LOGIC_TABLE_ROW_NBR=57        DEFAULT: (NO ABEND)                     
*ABEND_ON_MESSAGE_NBR=149               DEFAULT: (NO ABEND)                     
*EXECUTE_IN_MAIN_TASK=1                 DEFAULT: N                              
*                                           1=1ST UNIT, A=ALL UNITS             
//*                                                                             
//REFRTPRM DD *                                                                 
*TRACE KEYWORDS:                                                                
*VIEW=,FROMREC=,THRUREC=,FROMLTROW=,THRULTROW=,LTFUNC=,DDNAME=                  
*VPOS=,VLEN=,VALUE=                                                             
*                                                                               
*TRACE EXAMPLES:                                                                
*VIEW=4418,LTFUNC=WR                                                            
*VPOS=17,VLEN=10,VALUE=24CXA01501,VIEW=3424                                     
*VIEW=302                                                                       
//*                                                                             
//REFRENVV DD *                                                                 
//*                                                                             
//MR95VDP  DD DSN=&HLQ..&MLQ..PASS1C1.VDP,                                      
//            DISP=SHR                                                          
//*                                                                             
//REFRLTBL DD DSN=&HLQ..&MLQ..PASS1C1.JLT,                                      
//            DISP=SHR                                                          
//*                                                                             
//*        <<< INPUT REFERENCE FILES >>>                                        
//*                                                                 %%%         
//*            NONE                                                             
//*                                                                             
//*        <<< OUTPUT GENEVAERS REFERENCE WORK FILES >>>                        
//*                                                                 %%%         
//REFRREH  DD DSN=&HLQ..&MLQ..PASS1D1.REH,                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=SYSDA,                                                       
//            SPACE=(TRK,(10,1),RLSE),                                          
//            DCB=(DSORG=PS,RECFM=FB,LRECL=100)                                 
//*                                                                             
//REFRRTH  DD DSN=&HLQ..&MLQ..PASS1D1.RTH,                                      
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=SYSDA,                                                       
//            SPACE=(TRK,(10,1),RLSE),                                          
//            DCB=(DSORG=PS,RECFM=FB,LRECL=100)                                 
//*                                                                             
//REFR001  DD DSN=&HLQ..&MLQ..PASS1D1.FILE001.RED,                              
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=(SYSDA,10),                                                  
//            SPACE=(TRK,(1,1000),RLSE),                                        
//            DCB=(DSORG=PS,RECFM=VB,LRECL=4144)                                
//*                                                                             
//REFR002  DD DSN=&HLQ..&MLQ..PASS1D1.FILE002.RED,                              
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=(SYSDA,10),                                                  
//            SPACE=(TRK,(1,1000),RLSE),                                        
//            DCB=(DSORG=PS,RECFM=VB,LRECL=4144)                                
//*                                                                             
//REFR003  DD DSN=&HLQ..&MLQ..PASS1D1.FILE003.RED,                              
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=(SYSDA,10),                                                  
//            SPACE=(TRK,(1,1000),RLSE),                                        
//            DCB=(DSORG=PS,RECFM=VB,LRECL=4144)                                
//*                                                                             
//REFR004  DD DSN=&HLQ..&MLQ..PASS1D1.FILE004.RED,                              
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=(SYSDA,10),                                                  
//            SPACE=(TRK,(1,1000),RLSE),                                        
//            DCB=(DSORG=PS,RECFM=VB,LRECL=4144)                                
//*                                                                             
//REFR005  DD DSN=&HLQ..&MLQ..PASS1D1.FILE005.RED,                              
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=(SYSDA,10),                                                  
//            SPACE=(TRK,(1,1000),RLSE),                                        
//            DCB=(DSORG=PS,RECFM=VB,LRECL=4144)                                
//*                                                                             
//REFR006  DD DSN=&HLQ..&MLQ..PASS1D1.FILE006.RED,                              
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=(SYSDA,10),                                                  
//            SPACE=(TRK,(1,1000),RLSE),                                        
//            DCB=(DSORG=PS,RECFM=VB,LRECL=4144)                                
//*                                                                             
//REFRRPT  DD SYSOUT=*                                                          
//REFRLOG  DD SYSOUT=*                                                          
//REFRTRAC DD SYSOUT=*                                                          
//SNAPDATA DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//*                                                                             
//SYSMDUMP DD DSN=&HLQ..&MLQ..PASS1D1.SYSMDUMP,                                 
//            DISP=(NEW,DELETE,CATLG),                                          
//            UNIT=SYSDA,                                                       
//            SPACE=(TRK,(1000,1000),RLSE),                                     
//            DCB=(DSORG=PS,RECFM=FBS,LRECL=4160)                               
//*                                                                             
//*********************************************************************         
//* IF THE PRIOR STEP SETS AN INVALID RETURN CODE, TERMINATE THE JOB            
//*********************************************************************         
//*                                                                             
//PSTEP506 EXEC PGM=GVBUT99,                                                    
//            COND=(1,GE,PSTEP505),                                             
//            PARM='1099'                                                       
//*                                                                             
//*********************************************************************         
//* DELETE THE FILE(S) CREATED IN NEXT STEP                                     
//*********************************************************************         
//*                                                                             
//PSTEP700 EXEC PGM=IDCAMS                                                      
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
//SYSIN    DD *,SYMBOLS=EXECSYS                                                 
 /* VIEW DATA SETS: */                                                          
                                                                                
 /* DATA SETS  GOING TO FORMAT PHASE */                                         
                                                                                
 DELETE  &HLQ..&MLQ..PASS1E1.FILE005.EXT PURGE                                  
 DELETE  &HLQ..&MLQ..PASS1E1.FILE005.SXT PURGE                                  
 DELETE  &HLQ..&MLQ..PASS1E1.SYSMDUMP PURGE                                     
                                                                                
 IF LASTCC > 0 THEN        /* IF OPERATION FAILED,     */    -                  
     SET MAXCC = 0          /* PROCEED AS NORMAL ANYWAY */                      
                                                                                
//*                                                                             
//*********************************************************************         
//* EXTRACT DATA FOR VIEWS                                                      
//*********************************************************************         
//*                                                                             
//PSTEP705 EXEC PGM=GVBMR95E,                                                   
// REGION=0M                                                                    
//*                                                                             
//*        <<< INPUT GENEVAERS FILES >>>                                        
//*                                                                             
//EXTRPARM DD *                                                                 
*                                                                               
*   STANDARD OPTIONS                                                            
*-------------------                                                            
*                                                                               
*RUN_DATE=20170105                      DEFAULT: (CURRENT DATE)                 
*FISCAL_DATE_DEFAULT=20161231           DEFAULT: RUN_DATE                       
*FISCAL_DATE_OVERRIDE=1:20160731        DEFAULT: FISCAL_DATE_DEFAULT            
*                                                                               
*DISK_THREAD_LIMIT=2                    DEFAULT: 9999                           
*TAPE_THREAD_LIMIT=10                   DEFAULT: 9999                           
*                                                                               
*IO_BUFFER_LEVEL=8                      DEFAULT: 4                              
*OPTIMIZE_PACKED_OUTPUT=N               DEFAULT: Y                              
*PAGE_FIX_IO_BUFFERS=N                  DEFAULT: Y                              
*TREAT_MISSING_VIEW_OUTPUTS_AS_DUMMY=Y  DEFAULT: N                              
*ABEND_ON_CALCULATION_OVERFLOW=N        DEFAULT: Y                              
*                                                                               
*   DEBUGGING OPTIONS                                                           
*--------------------                                                           
*                                                                               
*TRACE=Y                                DEFAULT: N                              
*DUMP_LT_AND_GENERATED_CODE=Y           DEFAULT: N                              
*SOURCE_RECORD_LIMIT=100                DEFAULT: (NO LIMIT)                     
*ABEND_ON_LOGIC_TABLE_ROW_NBR=57        DEFAULT: (NO ABEND)                     
*ABEND_ON_MESSAGE_NBR=149               DEFAULT: (NO ABEND)                     
*EXECUTE_IN_PARENT_THREAD=A             DEFAULT: N                              
*                                           1=1ST UNIT, A=ALL UNITS             
//*                                                                             
//EXTRTPRM DD *                                                                 
*TRACE KEYWORDS:                                                                
*VIEW=,FROMREC=,THRUREC=,FROMLTROW=,THRULTROW=,LTFUNC=,DDNAME=                  
*VPOS=,VLEN=,VALUE=                                                             
*                                                                               
*TRACE EXAMPLES:                                                                
*VIEW=4418,LTFUNC=WR                                                            
*VPOS=17,VLEN=10,VALUE=24CXA01501,VIEW=3424                                     
*VIEW=302                                                                       
//*                                                                             
//EXTRENVV DD *                                                                 
$SUBSYS=DM12                                                                    
$QUAL=SDATRT01                                                                  
//*                                                                             
//MR95VDP  DD DSN=&HLQ..&MLQ..PASS1C1.VDP,                                      
//            DISP=SHR                                                          
//*                                                                             
//EXTRLTBL DD DSN=&HLQ..&MLQ..PASS1C1.XLT,                                      
//            DISP=SHR                                                          
//*                                                                             
//EXTRREH  DD DSN=&HLQ..&MLQ..PASS1D1.REH,                                      
//            DISP=SHR                                                          
//*                                                                             
//*        <<< INPUT GENEVAERS REFERENCE WORK FILES >>>                         
//*                                                                 %%%         
//REFR001  DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1D1.FILE001.RED                      
//REFR002  DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1D1.FILE002.RED                      
//REFR003  DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1D1.FILE003.RED                      
//REFR004  DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1D1.FILE004.RED                      
//REFR005  DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1D1.FILE005.RED                      
//REFR006  DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1D1.FILE006.RED                      
//*                                                                             
//*        <<< INPUT SOURCE FILES >>>                                           
//*                                                                 %%%         
//DDDATAF  DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1F0.F0010720.SET1                    
//         DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1F0.F0010722.SET4                    
//*                                                                             
//*        <<< OUTPUT FILES GOING TO FORMAT PHASE >>>                           
//*                                                                             
//EXTR005  DD DSN=&HLQ..&MLQ..PASS1E1.FILE005.EXT,                              
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=(SYSDA,10),                                                  
//            SPACE=(TRK,(1,1000),RLSE),                                        
//            DCB=(DSORG=PS,RECFM=VB,LRECL=8192)                                
//*                                                                             
//SORT005  DD DSN=&HLQ..&MLQ..PASS1E1.FILE005.SXT,                              
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=SYSDA,                                                       
//            SPACE=(TRK,(1,1),RLSE),                                           
//            DCB=(DSORG=PS,RECFM=FB,LRECL=80)                                  
//*                                                                             
//*        <<< OUTPUT EXTRACT VIEW FILES >>>                                    
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//EXTRRPT  DD SYSOUT=*                                                          
//EXTRLOG  DD SYSOUT=*                                                          
//EXTRTRAC DD SYSOUT=*                                                          
//MERGRPT  DD SYSOUT=*                                                          
//SNAPDATA DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//CEEDUMP  DD SYSOUT=*                                                          
//*                                                                             
//IDIOFF   DD DUMMY                                                             
//*                                                                             
//SYSUDUMP DD SYSOUT=*                                                          
//*SYSMDUMP DD DSN=&HLQ..&MLQ..PASS1E1.SYSMDUMP,                                
//*            DISP=(NEW,DELETE,CATLG),                                         
//*            UNIT=SYSDA,                                                      
//*            SPACE=(TRK,(1000,1000),RLSE),                                    
//*            DCB=(DSORG=PS,RECFM=FBS,LRECL=4160)                              
//*                                                                             
//*********************************************************************         
//* IF THE PRIOR STEP SETS AN INVALID RETURN CODE, TERMINATE THE JOB            
//*********************************************************************         
//*                                                                             
//PSTEP706 EXEC PGM=GVBUT99,                                                    
//            COND=(1,GE,PSTEP705),                                             
//            PARM='1099'                                                       
//*                                                                             
//*******************************************************************           
//* JSTEPNX1 - SUBMIT NEXT JOB: FORMAT JOB 1                                    
//*******************************************************************           
//*                                                                             
//JSTEPNX1 EXEC PGM=IEBGENER                                                    
//*                                                                             
//SYSIN    DD DUMMY                                                             
//*                                                                             
//SYSUT1   DD DSN=&HLQ..&MLQ..JCL(DEMOXF4),                                     
//            DISP=SHR                                                          
//*                                                                             
//SYSUT2   DD SYSOUT=(*,INTRDR)                                                 
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
//                                                                              
