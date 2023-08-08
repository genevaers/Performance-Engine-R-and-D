//NDELETEF JOB (ACCT),'DELETE FILE(S)',                                         
//            NOTIFY=&SYSUID.,                                                  
//            CLASS=A,                                                          
//            MSGLEVEL=(1,1),                                                   
//            MSGCLASS=X                                                        
//*                                                                             
//         EXPORT SYMLIST=*                                                     
//*                                                                             
//         SET HLQ=GEBT                                                         
//         SET MLQ=XYZ                                                          
//         SET LLQ=ACBDEFGH.B5*.**                                              
//*                                                                             
//PSTEPDEL EXEC PGM=IDCAMS                                                      
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
//SYSIN    DD *,SYMBOLS=EXECSYS                                                 
                                                                                
 DELETE  &HLQ..&MLQ..&LLQ MASK PURGE                                            
                                                                                
//*                                                                             
//                                                                              
//                                                                              
 IF MAXCC LE 8 THEN         /* IF OPERATION FAILED,     */    -                 
     SET MAXCC = 0          /* PROCEED AS NORMAL ANYWAY */                      
                                                                                
//*                                                                             
