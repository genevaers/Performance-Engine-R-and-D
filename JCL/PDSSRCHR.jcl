//NCBSRCHR JOB (ACCT),'SCAN PDS',                                       JOB20452
//            NOTIFY=&SYSUID.,
//            CLASS=A,
//            MSGLEVEL=(1,1),
//            MSGCLASS=H
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
//         SET LVL1=GEBT
//         SET RTC=22964
//         SET SCANDS=GEBT.LATEST.ASM
//*
//JOBLIB   DD DISP=SHR,DSN=&LVL1..RTC&RTC..GVBLOAD
//*
//*********************************************************************
//*  READ PDSE DIRECTORY AREA USING QSAM AND BUILDS MEMBER LIST
//*
//* This writes a list of PDSE members to DDNAME DDDRUCK.
//*
//*********************************************************************
//PDSSRCHR PROC
//PDSEUDUR EXEC PGM=GVBUDIR
//*
//PDSEINDD DD DISP=OLD,DSN=&SCANDS,
//            DCB=BLKSIZE=256
//DDDRUCK  DD DSN=&&MEMBERS,
//            DISP=(NEW,PASS),
//            UNIT=SYSDA,
//            SPACE=(TRK,(3,2),RLSE),
//            DCB=(DSORG=PS,RECFM=FB,LRECL=80)
//DDPRINT  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
//SYSUDUMP DD SYSOUT=*
//*
//*********************************************************************
//*  SCAN PDSE MEMBERS USING READ (BLOCK I/O) FROM LIST OF MEMBERS
//*
//* This looks for suspiciously translated characters in source code,
//* etc. which could result in an incorrect assembly/compilation.
//*
//*********************************************************************
//*
//PDSESCAN EXEC PGM=GVBUPDS
//*
//DDMEMIN  DD DISP=SHR,DSN=&SCANDS
//*
//DDDIRIN  DD DISP=(OLD,DELETE),DSN=&&MEMBERS
//*
//DDPRINT  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
//SYSUDUMP DD SYSOUT=*
//*
//         PEND
//*
//*        PUT YOUR PDS/PDSE LIST HERE
//*
//SRCH1    EXEC PDSSRCHR,SCANDS=GEBT.LATEST.ASM
//SRCH2    EXEC PDSSRCHR,SCANDS=GEBT.LATEST.COPYASM
//
