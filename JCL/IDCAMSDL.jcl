//NDELETEF JOB (ACCT),'DELETE FILE(S)',                                         
//            NOTIFY=&SYSUID.,                                                  
//            CLASS=A,                                                          
//            MSGLEVEL=(1,1),                                                   
//            MSGCLASS=X                                                        
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
