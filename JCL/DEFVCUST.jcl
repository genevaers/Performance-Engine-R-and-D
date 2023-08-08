//VSAMDEFN JOB (ACCT),'GENEVAERS DEF VSAM',                                     
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
//****************************************************************              
//*  Define VSAM cluster to contain customer name file                          
//****************************************************************              
//DEFINE   EXEC PGM=IDCAMS                                                      
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
//SYSIN    DD *,SYMBOLS=EXECSYS                                                 
  DEFINE CLUSTER(NAME(&HLQ..&MLQ..CUSTNAME.CLUSTER) -                           
    INDEXED KEYS (10,0) -                                                       
    RECORDSIZE (96,100) -                                                       
    UNIQUE -                                                                    
    VOLUMES (SAFR06) -                                                          
    SHAREOPTIONS (4,3) -                                                        
    FREESPACE (50,50) -                                                         
    FOR(999) -                                                                  
    CYLINDERS (18 3)) -                                                         
    DATA (CONTROLINTERVALSIZE (8192) -                                          
    NAME (&HLQ..&MLQ..CUSTNAME.DATA)) -                                         
    INDEX (NAME(&HLQ..&MLQ..CUSTNAME.INDEX))                                    
