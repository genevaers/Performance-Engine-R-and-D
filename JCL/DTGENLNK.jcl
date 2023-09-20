//DTGENLNK JOB (ACCT),'GENEVAERS LNK DTGEN',
//          NOTIFY=&SYSUID.,
//          REGION=0M,
//          CLASS=A,
//          MSGLEVEL=(1,1),
//          MSGCLASS=X
//********************************************************************    
//*                                                                       
//* (C) COPYRIGHT IBM CORPORATION 2021.                                   
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
//*
//*     LINK GVBDTGEN TEST DATA GENERATOR
//*
//* BEFORE SUBMITTING THIS JOB, PLEASE:
//*
//*     1)  UPDATE THE JOB STATEMENT ABOVE TO CONFORM TO YOUR
//*         INSTALLATION'S STANDARDS.
//*
//*     2)  SET THE VALUE OF "HLQ" ABOVE TO YOUR TSO PREFIX.
//*         THIS IS NORMALLY THE SAME AS YOUR TSO ID,
//*         UNLESS YOU HAVE CHANGED IT WITH THE TSO PROFILE PREFIX
//*         COMMAND.
//*
//*         THIS VALUE WILL DETERMINE THE HIGH-LEVEL QUALIFIER
//*         OF THE NAMES OF THE DEMO DATA SETS.
//*
//*     3)  THE "MLQ" DEFAULT VALUE IS GVBDEMO AND DOES NOT NEED TO BE
//*         CHANGED.
//*
//*********************************************************************
//*
//         EXPORT SYMLIST=*
//*
//*        SET HLQ=<YOUR-TSO-PREFIX>
//         SET MLQ=GVBDEMO
//*
//*********************************************************************
//*
//GVBDTGEN EXEC PGM=IEWL,
// PARM=(XREF,LET,LIST,MAP,RMODE(SPLIT),HOBSET,AC(1))
//*
//SYSLIN   DD DISP=SHR,DSN=&HLQ..&MLQ..BTCHOBJ(GVBDTGEN)
//         DD *
 NAME GVBDTGEN(R)
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
//SYSLMOD  DD DSN=&HLQ..&MLQ..BTCHLOAD(GVBDTGEN),
//            DISP=SHR
//*
//SYSPRINT DD SYSOUT=*
//*
