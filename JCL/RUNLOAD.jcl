//LOADVSAM JOB (ACCT),'GENEVAERS LOAD VSAM',                                    
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
//JOBLIB   DD DISP=SHR,DSN=&HLQ..&MLQ..GVBLOAD                                  
//*                                                                             
//*********************************************************************         
//*   RUN COBOL PROGRAM TO BROWSE VSAM FILE LOADED                              
//*********************************************************************         
//*                                                                             
//COB      EXEC PGM=MLOADVS,                                                    
// REGION=4096K,                                                                
// PARM='LIB,APOST,ARITH(EXTEND)/TERMTHDACT(UAIMM),TRAP(ON,NOSPIE)'             
//*                                                                             
//STEPLIB  DD DISP=SHR,DSN=&HLQ..&MLQ..EXITS.GVBLOAD                            
//         DD DISP=SHR,DSN=&HLQ..&MLQ..GVBLOAD                                  
//         DD DISP=SHR,DSN=IGY.V4R2M0.SIGYCOMP                                  
//         DD DISP=SHR,DSN=CEE.SCEERUN                                          
//*                                                                             
//*        Input dataset
//*
//CUSTNAMS DD DISP=(SHR,KEEP,KEEP),DSN=&HLQ..&MLQ..CUSTNAME                     
//*
//*        Output dataset
//*
//CUSTNAMV DD DISP=(SHR,KEEP,KEEP),DSN=&HLQ..&MLQ..CUSTNAME.CLUSTER             
//*                                                                             
//SYSIN    DD DUMMY                                                             
//SYSPRINT DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//SYSTERM  DD SYSOUT=*                                                          
//SYSUDUMP DD SYSOUT=*                                                          
//*                                                                             
