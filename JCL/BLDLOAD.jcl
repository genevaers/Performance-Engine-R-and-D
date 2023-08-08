//BLDLOAD  JOB (ACCT),'GENEVAERS BUILD COB',                                    
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
//*********************************************************************         
//*   COMPILE AND LINK COBOL PROGRAM TO LOAD VSAM TEST DATA                     
//*********************************************************************         
//*                                                                             
//JOBLIB   DD DISP=SHR,DSN=&HLQ..&MLQ..GVBLOAD                                  
//*                                                                             
//*********************************************************************         
//*   COMPILE COBOL PROGRAM                                                     
//*********************************************************************         
//*                                                                             
//COB      EXEC PGM=IGYCRCTL,                                                   
// PARM='LIB,APOST,ARITH(EXTEND),LIST',                                         
// REGION=4096K                                                                 
//*                                                                             
//STEPLIB  DD DISP=SHR,DSN=CEE.SCEERUN                                          
//         DD DISP=SHR,DSN=IGY.V4R2M0.SIGYCOMP                                  
//*                                                                             
//SYSIN    DD DSN=&HLQ..&MLQ..EXITS.COBOL(MLOADVS),DISP=SHR                     
//*                                                                             
//SYSLIB   DD DSN=&HLQ..&MLQ..EXITS.COPY,DISP=SHR                               
//         DD DSN=&HLQ..&MLQ..COPY,DISP=SHR                                     
//*                                                                             
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(5,1),RLSE)                                 
//SYSUT2   DD UNIT=SYSDA,SPACE=(CYL,(5,1),RLSE)                                 
//SYSUT3   DD UNIT=SYSDA,SPACE=(CYL,(5,1),RLSE)                                 
//SYSUT4   DD UNIT=SYSDA,SPACE=(CYL,(5,1),RLSE)                                 
//SYSUT5   DD UNIT=SYSDA,SPACE=(CYL,(5,1),RLSE)                                 
//SYSUT6   DD UNIT=SYSDA,SPACE=(CYL,(5,1),RLSE)                                 
//SYSUT7   DD UNIT=SYSDA,SPACE=(CYL,(5,1),RLSE)                                 
//*                                                                             
//SYSLIN   DD DSN=&&OBJECT,                                                     
//            DISP=(NEW,PASS),                                                  
//            UNIT=SYSDA,                                                       
//            SPACE=(TRK,(25,10),RLSE),                                         
//            RECFM=FB,LRECL=80,BLKSIZE=2960                                    
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
//SYSTERM  DD SYSOUT=*                                                          
//SYSUDUMP DD SYSOUT=*                                                          
//*                                                                             
//*********************************************************************         
//*   ABEND IF A PRIOR PROCESS HAS FAILED                                       
//*                                                                             
//*   NOTE: CHECK FOR ERROR MESSAGE ON THE PRIOR STEP                           
//*         FOR PROBLEM DETERMINATION                                           
//*********************************************************************         
//*                                                                             
//ABNDCOB  EXEC PGM=GVBUT99,                                                    
//            PARM='1099',                                                      
//            COND=(8,GT,COB)                                                   
//**********************************************************************        
//*  LINK-EDIT                                                                  
//**********************************************************************        
//*                                                                             
//LINK     EXEC PGM=IEWL,                                                       
// PARM=(XREF,LET,LIST,MAP,AMODE(31),RMODE(ANY),REUS(RENT))                     
//*                                                                             
//SYSLIN   DD DISP=SHR,DSN=&&OBJECT                                             
//*                                                                             
//SYSLIB   DD DISP=SHR,DSN=CEE.SCEERUN                                          
//         DD DISP=SHR,DSN=CEE.SCEELKED                                         
//         DD DISP=SHR,DSN=CEE.SCEELIB                                          
//         DD DISP=SHR,DSN=SYS1.CSSLIB                                          
//         DD DISP=SHR,DSN=SYS1.LINKLIB                                         
//*                                                                             
//SYSUT1   DD DSN=&&SYSUT1,                                                     
//            UNIT=SYSDA,                                                       
//            SPACE=(1024,(120,120),,,ROUND),                                   
//            BUFNO=1                                                           
//*                                                                             
//SYSLMOD  DD DSN=&HLQ..&MLQ..EXITS.GVBLOAD(MLOADVS),                           
//            DISP=SHR                                                          
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
//*********************************************************************         
//*   ABEND IF A PRIOR PROCESS HAS FAILED                                       
//*                                                                             
//*   NOTE: CHECK FOR ERROR MESSAGE ON THE PRIOR STEP                           
//*         FOR PROBLEM DETERMINATION                                           
//*********************************************************************         
//*                                                                             
//ABNDLINK EXEC PGM=GVBUT99,                                                    
//            PARM='1099',                                                      
//            COND=(8,GT,LINK)                                                  
//                                                                              
