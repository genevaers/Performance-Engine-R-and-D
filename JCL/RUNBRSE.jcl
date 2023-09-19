//BRSEVSAM JOB (ACCT),'GENEVAERS BRSE VSAM',                                    
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
//COB      EXEC PGM=MBRSEVS,                                                    
// REGION=4096K,                                                                
// PARM='LIB,APOST,ARITH(EXTEND)/TERMTHDACT(UAIMM),TRAP(ON,NOSPIE)'             
//*                                                                             
//STEPLIB  DD DISP=SHR,DSN=&HLQ..&MLQ..EXITS.GVBLOAD                            
//         DD DISP=SHR,DSN=&HLQ..&MLQ..GVBLOAD                                  
//         DD DISP=SHR,DSN=IGY.V4R2M0.SIGYCOMP                                  
//         DD DISP=SHR,DSN=CEE.SCEERUN                                          
//*                                                                             
//CUSTNAMV DD DISP=(SHR,KEEP,KEEP),DSN=&HLQ..&MLQ..CUSTNAME.CLUSTER             
//*                                                                             
//SYSIN    DD DUMMY                                                             
//SYSPRINT DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//SYSTERM  DD SYSOUT=*                                                          
//SYSUDUMP DD SYSOUT=*                                                          
//*                                                                             
