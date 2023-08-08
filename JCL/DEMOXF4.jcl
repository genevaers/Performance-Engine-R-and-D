//NCBDEMF4 JOB (ACCT),'SAFR FORMAT PHASE   ',                                   
//          NOTIFY=&SYSUID.,                                                    
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
//*   SORT, SUMMARIZE, AND FORMAT VIEWS                                         
//*                                                                             
//*********************************************************************         
//*                                                                             
//*********************************************************************         
//* PSTEP200 - DELETE THE FILE(S) CREATED IN NEXT STEP                          
//*                                                                             
//* NORMAL RETURN CODE = 0                                                      
//*                                                                             
//* IF THIS STEP ABENDS:                                                        
//* 1) CORRECT APPLICATION PROBLEM IF POSSIBLE                                  
//* 2) RESTART THE JOB IN THIS STEP                                             
//* 3) IF PROBLEM CANNOT BE CORRECTED, CONTACT APPLICATION SUPPORT              
//*                                                                             
//*********************************************************************         
//*                                                                             
//PSTEP200 EXEC PGM=IDCAMS                                                      
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
//SYSIN    DD *,SYMBOLS=EXECSYS                                                 
 /* VIEW DATA SETS: */                                                          
                                                                                
 DELETE  &HLQ..&MLQ..PASS1F0.F0010723.SET5   PURGE                              
                                                                                
 /* SAFR DATA SETS */                                                           
                                                                                
 DELETE  &HLQ..&MLQ..PASS1F0.SYSMDUMP PURGE                                     
                                                                                
 IF LASTCC > 0 THEN        /* IF OPERATION FAILED,     */    -                  
     SET MAXCC = 0          /* PROCEED AS NORMAL ANYWAY */                      
                                                                                
//*                                                                             
//*********************************************************************         
//* PSTEP205 - SUMMARIZE AND FORMAT VIEWS                                       
//*                                                                             
//* NORMAL RETURN CODES:                                                        
//*                                                                             
//*    0 = SUCCESSFUL, MARGINAL (HXM) FILE LOADED                               
//*    1 = SUCCESSFUL, MARGINAL (HXM) FILE EMPTY                                
//*                                                                             
//* IF THIS STEP ABENDS:                                                        
//* 1) CORRECT APPLICATION PROBLEM IF POSSIBLE                                  
//* 2) RESTART THE JOB IN PSTEP200                                              
//* 3) IF PROBLEM CANNOT BE CORRECTED, CONTACT APPLICATION SUPPORT              
//*                                                                             
//*********************************************************************         
//*                                                                             
//PSTEP205 EXEC PGM=GVBMR88,                                                    
// REGION=0M                                                                    
//*                                                                             
//*        <<< INPUT SAFR FILES >>>                                             
//*                                                                             
//MR88PARM DD *                                                                 
*RUN_DATE=20180416                      DEFAULT: (CURRENT DATE)                 
*FISCAL_DATE_DEFAULT=20180416           DEFAULT: RUN_DATE                       
*FISCAL_DATE_OVERRIDE=1:20190701        DEFAULT: FISCAL_DATE_DEFAULT            
*                                                                               
*ABEND_ON_CALCULATION_OVERFLOW=N        DEFAULT: Y                              
*ABEND_ON_DIVISION_BY_ZERO=N            DEFAULT: Y                              
*PROCESS_HEADER_RECORDS=N               DEFAULT: Y                              
*SORT_EXTRACT_FILE=N                    DEFAULT: Y                              
//*                                                                             
//MR88VDP  DD DSN=&HLQ..&MLQ..PASS1C1.VDP,                                      
//            DISP=SHR                                                          
//*                                                                             
//SYSIN    DD DSN=&HLQ..&MLQ..PASS1E1.FILE005.SXT,                              
//            DISP=SHR                                                          
//*                                                                             
//SORTCNTL DD *                                                                 
 OPTION DYNALLOC=(SYSDA,5),MAINSIZE=32M,RESALL=1M,VLSHRT                        
//*                                                                             
//SORTIN   DD DSN=&HLQ..&MLQ..PASS1E1.FILE005.EXT,                              
//            DISP=SHR                                                          
//*                                                                             
//MR88HXE  DD DUMMY                                                             
//*                                                                             
//MR88MSTR DD DUMMY,BLKSIZE=2408                                                
//*                                                                             
//REFRRTH  DD DSN=&HLQ..&MLQ..PASS1D1.RTH,                                      
//            DISP=SHR                                                          
//*                                                                             
//*        <<< INPUT REFERENCE WORK FILES >>>                                   
//*                                                                 %%%         
//REFR001  DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1D1.FILE001.RED                      
//REFR002  DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1D1.FILE002.RED                      
//REFR003  DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1D1.FILE003.RED                      
//REFR004  DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1D1.FILE004.RED                      
//REFR005  DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1D1.FILE005.RED                      
//REFR006  DD DISP=SHR,DSN=&HLQ..&MLQ..PASS1D1.FILE006.RED                      
//*                                                                             
//*        <<< OUTPUT SAFR FILES >>>                                            
//*                                                                             
//MR88RPT  DD SYSOUT=*                                                          
//MR88LOG  DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//SORTDIAG DD SYSOUT=*                                                          
//*                                                                             
//MR88HXM  DD DUMMY,BLKSIZE=27998                                               
//*                                                                             
//MR88HXD  DD DUMMY,BLKSIZE=2458                                                
//*                                                                             
//SYSMDUMP DD DSN=&HLQ..&MLQ..PASS1F0.SYSMDUMP,                                 
//            DISP=(NEW,DELETE,CATLG),                                          
//            UNIT=SYSDA,                                                       
//            SPACE=(TRK,(1000,1000),RLSE),                                     
//            DCB=(DSORG=PS,RECFM=FBS,LRECL=4160)                               
//*                                                                             
//*        <<< OUTPUT VIEW FILES >>>                                            
//*                                                                 %%%         
//F0010723 DD DSN=&HLQ..&MLQ..PASS1F0.F0010723.SET5,                            
//            DISP=(NEW,CATLG,DELETE),                                          
//            UNIT=SYSDA,                                                       
//            SPACE=(TRK,(10,10),RLSE),                                         
//            DCB=(DSORG=PS,RECFM=FB,LRECL=131)                                 
//*           DCB=(DSORG=PS,RECFM=VB,LRECL=1004)                                
//*                                                                             
//*                                                                             
//*********************************************************************         
//* IF THIS STEP ABENDS:                                                        
//* 1) FOLLOW RESTART INSTRUCTIONS FROM STEP PSTEP205                           
//*                                                                             
//*********************************************************************         
//*                                                                             
//PSTEP209 EXEC PGM=GVBUT99,                                                    
//            COND=(1,GE,PSTEP205),                                             
//            PARM='1099'                                                       
//*                                                                             
