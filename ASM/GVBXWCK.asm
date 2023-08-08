         TITLE 'GVBXWCK - REMOVE 11 BYTE PREFIX'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2008, 2021.
*     Copyright Contributors to the GenevaERS Project.
* SPDX-License-Identifier: Apache-2.0
*
**********************************************************************
*
*  Licensed under the Apache License, Version 2.0 (the "License");
*  you may not use this file except in compliance with the License.
*  You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
*  Unless required by applicable law or agreed to in writing, software
*  distributed under the License is distributed on an "AS IS" BASIS,
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
*  or implied.
*  See the License for the specific language governing permissions
*  and limitations under the License.
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
***********************************************************************
*                                                                     *
*  MODULE DESCRIPTION:                                                *
*                                                                     *
*      - THIS MODULE WORKS AS A COMPANION WRITE EXIT FOR THE          *
*        GENEVA READ EXIT "GVBXRCK".                                  *
*                                                                     *
*      - THE STANDARD 11 BYTE PREFIX APPENDED BY THE READ EXIT        *
*        IS REMOVED BEFORE WRITING                                    *
*                                                                     *
*                                                                     *
*  GENEVA MODULES USED    : NONE                                      *
*                                                                     *
***********************************************************************
                        EJECT
***********************************************************************
*                                                                     *
*           MODULE RETURN CODES AND REGISTER DOCUMENTATION            *
*                                                                     *
***********************************************************************
*                                                                     *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            0  - FOUND                                               *
*            4  - NOT FOUND                                           *
*            8  -                                                     *
*           12  - DISABLE VIEW                                        *
*           16  - ABEND   JOB                                         *
*                                                                     *
*  PARAMETERS:                                                        *
*                                                                     *
*        R1:  PARAMETER LIST ADDRESS                                  *
*                                                                     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - TEMPORARY WORK    REGISTER                             *
*            - RETURN    CODE                                         *
*                                                                     *
*        R14 - TEMPORARY WORK    REGISTER                             *
*            - RETURN    ADDR                                         *
*                                                                     *
*        R13 - WORK AREA ADDRESS                                      *
*                                                                     *
*        R12 -                                                        *
*        R11 - PROGRAM    BASE   REGISTER                             *
*                                                                     *
*        R10 - SUBROUTINE CALL   RETURN   ADDRESS (1ST LEVEL)         *
*                                                                     *
*        R9  - PARAMETER  LIST   ADDRESS                              *
*                                                                     *
*        R8  - EVENT      RECORD ADDRESS                              *
*                                                                     *
*        R7  - EXTRACT    RECORD ADDRESS                              *
*                                                                     *
*        R6  -                                                        *
*        R5  -                                                        *
*        R4  -                                                        *
*        R3  -                                                        *
*        R2  -                                                        *
*                                                                     *
*        R1  - TEMPORARY WORK    REGISTER                             *
*            - PARAMETER LIST    ADDRESS             (UPON ENTRY)     *
*                                                                     *
*        R0  - TEMPORARY WORK    REGISTER                             *
*                                                                     *
***********************************************************************
         IHASAVER DSECT=YES,SAVER=NO,SAVF4SA=YES,TITLE=NO
                        EJECT
         COPY GVBX95PA
***********************************************************************
*                                                                     *
*       "GVBXWCK" - W O R K A R E A   D E F I N I T I O N             *
*                                                                     *
***********************************************************************
*
WORKAREA DSECT
*        DS   0D
*
SAVEAREA DS    xl(savf4sa_len)    Format 4 save area
*
         DS   0D
*
OUTREC   DS    CL8192             OUTPUT RECORD WITHOUT 11 BYTE PREFIX
*
Worklen      EQU   *-WORKAREA
*                                                                     *
***********************************************************************
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        E X T R A C T   R E C O R D   A R E A                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
EXTREC   DSECT                    EXTRACT RECORD  WORK   AREA
*
EXRECLEN DS    HL02
         DS    XL02
EXSORTLN DS    HL02               SORT    KEY     LENGTH
EXTITLLN DS    HL02               SORT    TITLE   LENGTH
EXDATALN DS    HL02               EXTRACT DATA    LENGTH
EXNCOL   DS    HL02               NUMBER  OF  CALCULATED COLUMNS ("CT")
EXVIEW#  DS    FL04               VIEW    NUMBER  (+X'80000000')
EXSRTKEY DS   0CL01
                     SPACE 3
         DS    CL8164             BUFFER  AREA    (8192-8-BDW-RDW-12)
*
                        EJECT
***********************************************************************
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
***********************************************************************
         YREGS
*
                        EJECT
         print off,nogen,noprint
         SYSSTATE ARCHLVL=2,amode64=YES
         COPY  ASMMSP
LEAVE    OPSYN ASM_LEAVE
         asmmrel on
         PRINT GEN,on
*
GVBXWCK  RMODE 31
GVBXWCK  AMODE 64
GVBXWCK  CSECT
         J     CODE
XWCKEYE  GVBEYE GVBXWCK
static   loctr            set up the static loctr
code     loctr            followed by the code loctr
prev     using savf4sa,r13          map the save area
         stmg  R14,R12,prev.SAVF4SAG64RS14 Save callers registers
         larl  r11,gvbxwck
         USING GVBXWCK,r11
*
         LA    R9,0(,R1)          LOAD  PARAMETER LIST    ADDRESS
         USING GENPARM,R9
*
         llgt  r14,gpenva         get enviroment info area ptr
         using genenv,r14           and map
         if    (cli,gpphase,eq,c'I')   Init  phase
           LHI R15,8           set return code
           j   RETURN_I           return for Init Phase
         endif
         if    (cli,gpphase,eq,c'T')   Term  phase
           LHI R15,8           set return code
           j   RETURN_I           return for Term Phase
         endif
*
         DROP  R14
*
*        Determine if it is the 1st time the exit has been call
*
         llgt  R2,GPWORKA
         LT    R1,0(,R2)               .1st time?
         jp    Chain                        .N:
*
*        Get Program Storage
Workallo DS    0H
         STORAGE OBTAIN,LENGTH=WORKLEN+8,LOC=31,COND=NO,               +
               CHECKZERO=YES
         IF cij,R15,NE,X'14'    NOT ZEROED?
           LGR R10,R1            SAVE ADDRESS
           LgR R0,R1             ZERO  WORK  AREA
           Lghi R1,WORKLEN+8
           SgR R14,R14
           SgR R15,R15
           MVCL R0,R14
           LGR R1,R10            RESTORE POINTER
         ENDIF
*
         MVC  0(L'WORKEYEB,R1),WORKEYEB
         AGHI R1,L'WORKEYEB     MOVE POINTER PAST
         USING WORKAREA,R1
         USING SAVF4SA,WORKAREA
         STG R13,SAVF4SAPREV     SAVE OLD SAVE AREA IN CURRENT
         STG R1,PREV.SAVF4SANEXT SAVE CURRENT SAVE AREA IN OLD
         MVC SAVF4SAID(4),=A(SAVF4SAID_VALUE) SET 'F4SA' IN AREA
         LGR R13,R1             GET NEW WORKAREA INTO R13
         ST R13,0(,R2)          SAVE  WORK AREA ADDRESS (POINTER)
*
         llgt  R14,GPENVA
         USING GENENV,R14
         if    (cli,gpphase,eq,c'O')   Open  phase
           j   RETURN_8           return for Open Phase
         endif
         DROP  R14
         bru   MAINLINE           Goto MAINLINE
*
*  CHAIN REGISTER SAVE AREAS TOGETHER
*
Chain    DS 0H
         STG   R13,SAVF4SAPREV     SET   FORWARD  POINTER IN OLD
         STG   R1,PREV.SAVF4SANEXT SET   BACKWARD POINTER IN NEW
         LGR   R13,R1           GET NEW WORKAREA INTO R13
         DROP R1
         DROP PREV
         USING WORKAREA,R13
         USING SAVF4SA,WORKAREA
         llgt  R14,GPENVA
         USING GENENV,R14
         if    (cli,gpphase,eq,c'C')   Close  phase
           j   RETURN_8           return for Close Phase
         endif
         DROP  R14
*
***********************************************************************
*  RETURN LOOK-UP KEY AS LOOK-UP RESULT                               *
***********************************************************************
MAINLINE DS    0H
         LLGT  R8,GPEVENTA        Load pointer to Event Record address
         LG    R8,0(,R8)          Load Event Record address
         aghi  R8,-4              subtract 4 to point to RDW
*
         llgt  R7,GPEXTRA         LOAD  EXTRACT   REC     ADDRESS
         USING EXTREC,R7
*
         LH    R15,EXDATALN       CHECK FOR END-OF-VIEW HEADER RECORD
         cijl  r15,0,RETURN_8     If EOV HDR Fnd branch to RETURN_I
*
***********************************************************************
*  COPY EVENT RECORD TO OUTPUT AREA                                   *
***********************************************************************
COPY     LH    R15,0(,R8)         LOAD  EXTRACT RECORD LENGTH
         AHI   R15,-11            REMOVE PREFIX LENGTH
         cijnh r15,0,RETURN0      If rec len <= 0 branch to RETURN0
*
         LA    R1,OUTREC          CHANGE POINTER TO COPIED RECORD
         llgt  R14,GPBLOCKA
         stg   R1,0(,R14)
*
         STH   R15,0(,R1)         BUILD  RDW less 11 bytes
         XC    2(2,R1),2(R1)
         LA    R1,4(,R1)
*
         AHI   R15,-4             REMOVE RDW
         LA    R14,4+11(,R8)      SKIP OVER RDW + 11 BYTE PREFIX
*
         LA    R15,255(,R15)      ROUND UP TO  256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO MOVE
         SRL   R0,8
         j     COPYEND
COPYLOOP MVC   0(256,R1),0(R14)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
COPYEND  BRCT  R0,COPYLOOP
         exrl  R15,MVCR1R14       UPDATE  EVENT  RECORD
*
*
***********************************************************************
*  RETURN TO CALLER (GENEVA)                                          *
***********************************************************************
RETURN0  Sgr   R15,R15            ZERO RETURN CODE
*
RETURN   lg    r13,savf4saprev         restore caller save area
*
RETURNRC llgt  R14,GPRTNCA        LOAD RETURN CODE  ADDRESS
         ST    R15,0(,R14)
*
         lg    r14,savf4sag64rs14      restore caller's r14
         lmg   r2,r12,savf4sag64rs2    restore caller's r2- r12
         BSM   0,R14              RETURN
*
RETURN_I DS    0H
         LHI   R15,8
         j     RETURNRC
*
RETURN_8 DS    0H
         LHI   R15,8
         j     RETURN
*
         DS    0D
MVCR1R14 MVC   0(0,R1),0(R14)     * * * * *  E X E C U T E D  * * * * *
*
         DROP  R13
*
                        EJECT
***********************************************************************
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
***********************************************************************
*
static   loctr            set up the static loctr
         DS    0D
*
WORKEYEB DC    CL8'XWCKWORK'
*
         LTORG
*
code     loctr            followed by the code loctr
         END
