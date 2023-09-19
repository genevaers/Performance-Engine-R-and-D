//VSAMDEFN JOB (ACCT),'GENEVAERS DEF VSAM',                                     
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
