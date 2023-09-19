//NBLDXR1  JOB (ACCT),'GENEVAERS BUILD ASM',                                    
//          NOTIFY=&SYSUID.,                                                    
//          REGION=0M,                                                          
//          CLASS=A,                                                            
//          MSGLEVEL=(1,1),                                                     
//          MSGCLASS=X                                                          
//********************************************************************    
//*                                                                       
//* (C) COPYRIGHT IBM CORPORATION 2023.                                   
//*    Copyright Contributors to the GenevaERS Project.                   
//*SPDX-License-Identifier: Apache-2.0                                    
//*                                                                       
//********************************************************************    
//*                                                                       
//*  Licensed under the Apache License, Version 2.0 (the "License");      
//*  you may not use this file except in compliance with the License.     
//*  You may obtain a copy of the License at                              
//*                                                                       
//*     http://www.apache.org/licenses/LICENSE-2.0                        
//*                                                                       
//*  Unless required by applicable law or agreed to in writing, software  
//*  distributed under the License is distributed on an "AS IS" BASIS,    
//*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express         
//*  or implied.                                                          
//*  See the License for the specific language governing permissions      
//*  and limitations under the License.                                   
//*                                                                       
//*********************************************************************         
//*  ASSEMBLE AND LINK ASSEMBLER GVBXR1 READ EXIT                   
//*********************************************************************         
//*                                                                             
//         EXPORT SYMLIST=*                                                     
//*                                                                             
//*        SET HLQ=<YOUR-TSO-PREFIX> 
//         SET MLQ=GVBDEMO                                                           
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
//SYSIN    DD DSN=&HLQ..&MLQ..EXITS.ASM(GVBXR1),                                
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
//*  LINK-EDIT GVBXR1      APF Authorized                                                     
//**********************************************************************        
//*                                                                             
//GVBXR1   EXEC PGM=IEWL,                                                       
// PARM=(AC(1),XREF,LET,LIST,MAP,AMODE(31),RMODE(24),REUS(RENT))                
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
//SYSLMOD  DD DSN=&HLQ..&MLQ..EXITS.GVBLOAD(GVBXR1),                                                             
//            DISP=SHR                                                          
//*                                                                             
//SYSPRINT DD SYSOUT=*                                                          
//*                                                                             
