//LSTVSAM  JOB (ACCT),'LIST VSAM FILE',                                         
//            NOTIFY=&SYSUID.,                                                  
//            CLASS=A,                                                          
//            MSGLEVEL=(1,1),                                                   
//            MSGCLASS=X                                                        
//*                                                                             
//         EXPORT SYMLIST=*                                                     
//*                                                                             
//*        SET HLQ=<YOUR-TSO-PREFIX>                                            
//         SET MLQ=GVBDEMO                                                      
//*                                                                             
//*                                                                             
//****************************************************************              
//*  List VSAM cluster                                                          
//****************************************************************              
//LSTVSAM  EXEC PGM=IDCAMS                                                      
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
//SYSIN    DD *,SYMBOLS=EXECSYS                                                 
  PRINT INDATASET(&HLQ..&MLQ..CUSTNAME.CLUSTER) -                               
        CHARACTER                                                               
//                                                                              
//                                                                              
//                                                                              
//*                                                                             
//****************************************************************              
//*  Delete VSAM cluster                                                        
//****************************************************************              
//DELVSAM  EXEC PGM=IDCAMS                                                      
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
//SYSIN    DD *,SYMBOLS=EXECSYS                                                 
  DELETE &HLQ..&MLQ..CUSTNAME.CLUSTER -                                         
         PURGE                                                                  
//                                                                              
