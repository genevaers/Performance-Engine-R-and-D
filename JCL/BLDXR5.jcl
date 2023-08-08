//NBLDXR5  JOB (ACCT),'GENEVAERS BUILD ASM',                                    
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
//*  ASSEMBLE AND LINK ASSEMBLER READ EXIT USING GVBTP90 QSAM                   
//*********************************************************************         
//*                                                                             
//JOBLIB   DD DISP=SHR,DSN=&HLQ..&MLQ..GVBLOAD                                  
//*                                                                             
//****************************************************************              
//*  ASSEMBLE MODULE                                                            
//****************************************************************              
//*                                                                             
//ASM      EXEC PGM=ASMA90,                                                     
// PARM=(NODECK,OBJECT,'SYSPARM(RELEASE)','OPTABLE(ZS7)',                       
// 'PC(GEN),FLAG(NOALIGN),SECTALGN(256),GOFF,LIST(133)')                        
//*                                                                             
//SYSIN    DD DSN=&HLQ..&MLQ..EXITS.ASM(GVBXR5),                                
//            DISP=SHR                                                          
//*                                                                             
//SYSLIB   DD DISP=SHR,DSN=&HLQ..&MLQ..EXITS.COPYASM                            
//         DD DISP=SHR,DSN=&HLQ..&MLQ..COPYASM                                  
//         DD DISP=SHR,DSN=ASM.SASMMAC2                                         
//         DD DISP=SHR,DSN=SYS1.MACLIB                                          
//         DD DISP=SHR,DSN=SYS1.MODGEN                                          
//         DD DISP=SHR,DSN=CEE.SCEEMAC                                          
//*                                                                             
//SYSLIN   DD DSN=&&OBJECT,                                                     
//            DISP=(NEW,PASS),                                                  
//            UNIT=SYSDA,                                                       
//            SPACE=(TRK,(25,10),RLSE),                                         
//            RECFM=FB,LRECL=80,BLKSIZE=2960                                    
//*                                                                             
//SYSUT1   DD DSN=&&SYSUT1,                                                     
//            UNIT=SYSDA,                                                       
//            SPACE=(1024,(300,300),,,ROUND),                                   
//            BUFNO=1                                                           
//*                                                                             
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
//ABNDASM  EXEC PGM=GVBUT99,                                                    
//            PARM='1099',                                                      
//            COND=(8,GT,ASM)                                                   
//*                                                                             
//**********************************************************************        
//*  LINK-EDIT GVBXR5                                                           
//**********************************************************************        
//*                                                                             
//GVBXR5   EXEC PGM=IEWL,                                                       
// PARM=(XREF,LET,LIST,MAP,AMODE(31),RMODE(24),REUS(RENT))                
//*                                                                             
//SYSLIN   DD DSN=&&OBJECT,                                                     
//            DISP=(OLD,DELETE)                                                 
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
//SYSLMOD  DD DSN=&HLQ..&MLQ..EXITS.GVBLOAD(GVBXR5),                            
//            DISP=SHR                                                          
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
