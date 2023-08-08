         TITLE 'GVBXWTRN - COPY "DT" AREA TO COMMON KEY LR BUFFER'
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
*      - THIS MODULE WORKS WITH THE COMMON KEY READ EXIT "GVBXRCK"    *
*                                                                     *
*      - THE "DT" AREA BUILT BY A VIEW IS COPIED INTO THE "GVBXRCK"   *
*        TRANSACTION BUFFER USING THE "GENCKTRN" ENTRY POINT.         *
*                                                                     *
*  GENEVA MODULES USED    : NONE                                      *
*                                                                     *
***********************************************************************
                        EJECT
*
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
*            8  - END-OF-FILE                                         *
*           12  - DISABLE   VIEW                                      *
*           16  - ABEND     JOB                                       *
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
*        R12 - PROGRAM    BASE   REGISTER                             *
*        R11 - PROGRAM    BASE   REGISTER                             *
*                                                                     *
*        R10 - SUBROUTINE CALL   RETURN   ADDRESS (1ST LEVEL)         *
*                                                                     *
*        R9  - WORK AREA  ANCHOR ADDRESS                              *
*                                                                     *
*        R8  - PARAMETER  LIST   ADDRESS                              *
*                                                                     *
*        R7  - EVENT FILE AREA   ADDRESS                              *
*        R6  - EXTRACT  RECORD   ADDRESS                              *
*        R5  - GENERATED TRANSACTION RECORD ADDRESS                   *
*                                                                     *
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
                        EJECT
         COPY GVBX95PA
***********************************************************************
*                                                                     *
*       "GVBXWTRN" - W O R K A R E A  D E F I N I T I O N             *
*                                                                     *
***********************************************************************
*
WORKAREA DSECT
*
SAVEAREA DS  18F                  REGISTER   SAVE AREA
*
DBLWORK  DS    D                  DOUBLEWORD WORK AREA
*
TRANRECA DS    A                  GENERATED  TRANSACTION  RECORD ADDR
TRANRECL DS    F                  GENERATED  TRANSACTION  RECORD LEN
TRANANCR DS    A                  "GENCKTRN" WORK   AREA  ANCHOR
TRANRTN  DS    F                  GENERATED  TRANSACTION  RECORD ADDR
*
GENCKADR DS    A                  "GENCKTRN" ENTRY POINT  ADDRESS
*
PARMAREA DS   4A                  NAME/TOKEN PARAMETER    LIST
*
WORKLEN  EQU   *-WORKAREA
                        EJECT
***********************************************************************
*                                                                     *
*        S T A R T U P   D A T A  (S T A T I C   T A R G E T   L R)   *
*                                                                     *
***********************************************************************
*
PARMDATA DSECT
*
PDENTTYP DS    CL08             OUTPUT ENTITY TYPE
         DS    CL24
PDDATALN EQU   *-PARMDATA
                        SPACE 3
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
                        EJECT
***********************************************************************
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
***********************************************************************
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
                        SPACE 3
***********************************************************************
*                                                                     *
*        REGISTER SAVE AREA OFFSETS:                                  *
*                                                                     *
***********************************************************************
*
RSABP    EQU   4
RSAFP    EQU   8
RSA14    EQU   12
RSA15    EQU   16
RSA0     EQU   20
RSA1     EQU   24
RSA2     EQU   28
*
                        EJECT
         PRINT GEN
*
GVBXWTRN RMODE ANY
GVBXWTRN AMODE 31
GVBXWTRN CSECT
         J     CODE
WTRNEYE  GVBEYE GVBXWTRN
*
CODE     STM   R14,R12,RSA14(R13) SAVE  CALLER'S  REGISTERS
*
         LA    R11,0(,R15)        SET   PROGRAM   BASE    REGISTER
         USING GVBXWTRN,R11
*
         LR    R8,R1              LOAD  PARAMETER LIST    ADDRESS
         USING GENPARM,R8
***********************************************************************
*  CHECK FOR INIT PHASE, RETURN RC=8 IF SO                            *
***********************************************************************
         L     R14,GPENVA         LOAD ENVIRONMENT INFO ADDRESS
         USING GENENV,R14
         CLI   GPPHASE,C'I'       TEST FOR INITIALISATION PHASE
         BRE   RETURN_I           RETURN
*
         CLI   GPPHASE,C'T'       TEST FOR INITIALISATION PHASE
         BRE   RETURN_I           RETURN
*
         L     R7,GPFILEA         LOAD  FILE      AREA    ADDRESS
         USING GENFILE,R7
*
         L     R6,GPEXTRA         LOAD  EXTRACT   RECORD  ADDRESS
         USING EXTREC,R6
                        SPACE 3
         LR    R10,R13            SAVE  CALLER'S  RSA     ADDRESS
*
***********************************************************************
*  CHECK FOR CLOSE PHASE, RETURN RC=8 IF SO                           *
***********************************************************************
         L     R14,GPENVA         LOAD ENVIRONMENT INFO ADDRESS
         USING GENENV,R14
         CLI   GPPHASE,C'C'       TEST FOR CLOSE   PHASE
         BRNE  OPEN               CONTINUE PROCESSING
*
         LHI   R15,8              SET  RETURN CODE
         L     R14,GPRTNCA        LOAD RETURN CODE ADDRESS
         ST    R15,0(,R14)
*
         LR    R13,R10            RESTORE  R13
         BRU   RETURNCL           RETURN
*
         DROP  R14
*
OPEN     L     R9,GPWORKA         LOAD  WORK AREA POINTER ADDRESS
         L     R13,0(,R9)         LOAD  POINTER   VALUE
         USING WORKAREA,R13
         LTR   R13,R13            ALLOCATED  ???
         BRP   CHAIN              YES - BYPASS ALLOCATION
*
***********************************************************************
*  ALLOCATE "GVBXWTRN" WORKAREA IF NOT ALREADY ALLOCATED (PER "WR")   *
***********************************************************************
WORKALLO LHI   R0,WORKLEN+8       LOAD  WORK AREA SIZE
*        GETMAIN R,LV=(0),LOC=(ANY)
         STORAGE OBTAIN,LENGTH=(0),COND=NO,CHECKZERO=YES
         MVC   0(8,R1),WORKEYEB
         LA    R13,8(,R1)
         ST    R13,0(,R9)         SAVE  WORK AREA ADDRESS (POINTER)
*
         LR    R0,R13             ZERO  WORK AREA
         LHI   R1,WORKLEN
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
         ST    R13,RSAFP(,R10)    SET   FORWARD  POINTER IN OLD
         ST    R10,RSABP(,R13)    SET   BACKWARD POINTER IN NEW
*                                                                     *
*        If SNAP is required, use SNAP=Y so that MR95 allocates and   *
*        opens the file.                                              *
*        The SNAPDCB address is saved in SNAPDCBA                     *
*                                                                     *
         BRAS  R10,INITWORK       INITIALIZE WORK   AREA
*
*        LA    R9,36-1(,R8)
*          ly  R2,SNAPDCBA
*        SNAP  DCB=(R2),ID=100,STORAGE=((R8),(R9))
*        LA    R9,WORKLEN-1(,R13)
*          ly  R2,SNAPDCBA
*        SNAP  DCB=(R2),PDATA=(REGS),ID=101,STORAGE=((R13),(R9))
*
***********************************************************************
*  CHECK FOR OPEN PHASE, RETURN IF SO                                 *
***********************************************************************
         L     R14,GPENVA         LOAD ENVIRONMENT INFO ADDRESS
         USING GENENV,R14
         CLI   GPPHASE,C'O'       TEST FOR OPEN PHASE
         BRE   RETURN0            CONTINUE PROCESSING
         DROP  R14
*
         BRU   TRANHDR
                        EJECT
***********************************************************************
*  CHAIN REGISTER SAVE AREAS TOGETHER                                 *
***********************************************************************
CHAIN    ST    R13,RSAFP(,R10)    SET   FORWARD  POINTER IN OLD
         ST    R10,RSABP(,R13)    SET   BACKWARD POINTER IN NEW
*
*        LA    R9,WORKLEN-1(,R13)
*          ly  R2,SNAPDCBA
*        SNAP  DCB=(R2),PDATA=(REGS),ID=102,STORAGE=((R13),(R9))
*
***********************************************************************
*  CHECK FOR END-OF-VIEW HEADER RECORD                                *
***********************************************************************
TRANHDR  LH    R0,EXDATALN        LOAD   DATA     LENGTH
         LTR   R0,R0              HEADER RECORD   ???
         BRP   TRANBUFR           NO  -  ALLOCATE BUFFER
*
         LHI   R15,8
         BRU   RETURN
*
***********************************************************************
*  ALLOCATE GENERATED TRANSACTION RECORD BUFFER (IF NOT ALLOCATED)    *
***********************************************************************
TRANBUFR L     R5,TRANRECA        LOAD  GENERATED TRAN REC BUFFER ADDR
         LTR   R5,R5
         BRP   TRANCOPY
*
         ST    R0,TRANRECL        SAVE  DATA   LENGTH
         AHI   R0,4+8             ADD   RDW  + EYEBALL LENGTH
*        GETMAIN R,LV=(0),LOC=(ANY)
         STORAGE OBTAIN,LENGTH=(0),COND=NO,CHECKZERO=YES
         MVC   0(8,R1),BUFREYEB
         LA    R5,8(,R1)
         ST    R5,TRANRECA        SAVE  TRAN BUFR ADDRESS
*
         ST    R5,PARMAREA+4      SET   REC  ADDR PARAMETER
*
         XC    0(4,R5),0(R5)      ZERO  RDW
*
***********************************************************************
*  COPY "DT" AREA TO GENERATED TRANSACTION RECORD BUFFER              *
***********************************************************************
TRANCOPY L     R15,TRANRECL
         LA    R0,4(,R15)         BUILD RDW
         STH   R0,0(,R5)
         LA    R5,4(,R5)
*
         LA    R14,EXSRTKEY
         AH    R14,EXSORTLN
         AH    R14,EXTITLLN
*
         LA    R15,255(,R15)
         LR    R0,R15
         SRL   R0,8
         BRU   TRANMVCT
TRANMVLP MVC   0(256,R5),0(R14)   COPY  256 BYTES
         LA    R5,256(,R5)        ADVANCE  TARGET
         LA    R14,256(,R14)      ADVANCE  SOURCE
TRANMVCT BRCT  R0,TRANMVLP
         EX    R15,MVCR5R14       COPY  REMAINDER
*
***********************************************************************
*                                                                     *
*   CALL GENCKTRN USING THREAD-DDNAME              PIC  X(8)       +0 *
*                       OUTPUT-REC                 PIC  X(?)       +4 *
*                       GENCKGEN-WORK-AREA-ANCHOR  POINTER         +8 *
*                       RETURN-CODE                PIC S9(8) COMP  +12*
***********************************************************************
         LA    R1,PARMAREA
         llgf  R15,GENCKADR
         BASSM R14,R15
*
         L     R15,TRANRTN
         LTR   R15,R15
         BRZ   RETURN8
*
         LHI   R15,16             ABORT RUN
         BRU   RETURN
*
MVCR5R14 MVC   0(0,R5),0(R14)     * * * * E X E C U T E D * * * *
*
                        EJECT
***********************************************************************
*  RETURN TO CALLER (GVBMR95)                                         *
***********************************************************************
RETURN0  SR    R15,R15            ZERO RETURN CODE
         BRU   RETURN
*
RETURN8  LHI   R15,8              ZERO RETURN CODE
*
RETURN   L     R14,GPRTNCA        LOAD RETURN CODE  ADDRESS
         ST    R15,0(,R14)
*
         L     R13,RSABP(,R13)    RESTORE REGISTER  R13
RETURNCL L     R14,RSA14(,R13)    RESTORE REGISTER  R14
         LM    R0,R12,RSA0(R13)   RESTORE REGISTERS R0 - R12
         BSM   0,R14              RETURN
*
RETURN_I DS    0H
         LHI   R15,8
         BRU   RETURNCL
                        EJECT
***********************************************************************
*                                                                     *
*        I N I T I A L I Z E   W O R K   A R E A                      *
*                                                                     *
***********************************************************************
         USING WORKAREA,R13
*
INITWORK LA    R14,GPDDNAME       INITIALIZE PARAMETER AREA
         ST    R14,PARMAREA+0
*
         LHI   R14,0
         ST    R14,PARMAREA+4
*
         LA    R14,TRANANCR
         ST    R14,PARMAREA+8
*
         LA    R14,TRANRTN
         ST    R14,PARMAREA+12
*
         LOAD  EP=GENCKTRN        "GENCKTRN" ENTRY POINT IN "GVBXRCK"
         ST    R0,GENCKADR
*
INITEXIT BR    R10                RETURN
                        EJECT
***********************************************************************
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
***********************************************************************
         DS   0D
MODE31   DC    XL4'80000000'
*
WORKEYEB DC    CL8'XWTRNWRK'
BUFREYEB DC    CL8'XWTRNREC'
                        SPACE 3
         LTORG
*
*
         END
