         TITLE 'GVBRD35 - BUILD GENEVA ERS REFERENCE DATA FILE'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2004, 2021.
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
*                                                                     *
*  GVBRD35  - BUILD GENEVA ERS COMPATIBLE REFERENCE DATA   FILE       *
*             AND A GENEVA ERS COMPATIBLE REFERENCE HEADER FILE       *
*                   (USING A TEMPLATE REH RECORD)                     *
*                                                                     *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            0  - SUCCESSFUL                                          *
*           16  - ERROR                                               *
*                                                                     *
*  PARAMETERS:                                                        *
*                                                                     *
*        R1:  SORT PARAMETER LIST ADDRESS                             *
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
*        R9  - SUBROUTINE CALL   RETURN   ADDRESS (2ND LEVEL)         *
*                                                                     *
*        R8  - PARAMETER LIST    ADDRESS                              *
*                                                                     *
*        R7  -                                                        *
*        R6  -                                                        *
*        R5  -                                                        *
*        R4  -                                                        *
*                                                                     *
*        R3  - "REH" DSECT  BASE REGISTER                             *
*        R2  - "REH" DCB ADDRESS                                      *
*                                                                     *
*        R1  - TEMPORARY WORK    REGISTER                             *
*            - PARAMETER LIST    ADDRESS             (UPON ENTRY)     *
*                                                                     *
*        R0  - TEMPORARY WORK    REGISTER                             *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*       "GVBRD35" - W O R K A R E A    D E F I N I T I O N            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
WORKAREA DSECT
*
SAVEAREA DS  18F               REGISTER    SAVE  AREA
*
DBLWORK  DS    D               DOUBLEWORD  WORK  AREA
*
SORTCNT  DS    PL8             "SORTOUT" RECORD  COUNT
*
OPENPARM DS    XL8             OPEN   PARAMETER  AREA
*
SORTRECF DS    XL2             "SORTOUT" RECORD  FORMAT
SORTRECL DS    XL2             "SORTOUT" RECORD  LENGTH
*
SORTOREC DS    A               "SORTOUT" RECORD  ADDRESS
*
HDRRECA  DS    A               "MR95REH" RECORD  ADDRESS
*
JFCBPARM DS    A               "RDJFCB"  PARAMETER  LIST
JFCBEXIT DS    A               DCB EXIT  LIST
*
SORTOUT  DS    (SORTDCBL)C     "SORTOUT" DCB
HDRFILE  DS    (REHDCBL)C      "MR95REH" DCB
*
         DS   0F
JFCB     DS    XL176           JOB FILE  CONTROL BLOCK
*
WORKLEN  EQU   *-WORKAREA
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        R E F E R E N C E   D A T A   H E A D E R   R E C O R D      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
TBLHEADR DSECT                 TABLE   DATA   HEADER RECORD
*
TBFILEID DS    FL04            FILE    ID
TBRECID  DS    FL04            LOGICAL RECORD ID
TBRECCNT DS    FL04            RECORD  COUNT
TBRECLEN DS    HL02            RECORD  LENGTH
TBKEYOFF DS    HL02            KEY     OFFSET
TBKEYLEN DS    HL02            KEY     LENGTH
TBXFILE# DS   0HL02            "JLT"   EXTRACT  FILE NUMBER
         DS    XL01            FILLER
TBEFFIND DS    CL01            EFFECTIVE DATE INDICATOR (VERSION 3)
         DS    XL01            FILLER
TBEFFDAT DS    XL01            EFFECTIVE DATE OPTION CODE
         DS    XL01            FILLER
TBTXTFLG DS    XL01            TEXT DATA FLAG
*
TBHDRLEN EQU   *-TBLHEADR      TABLE   HEADER LENGTH
*
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*       S O R T   E X I T   P A R A M E T E R   L I S T               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
PARMLIST DSECT
*
SORTIREC DS    A               +0  - INPUT    RECORD ADDRESS
SORTPREC DS    A               +4  - PREVIOUS OUTPUT RECORD  ADDRESS
SORTWORK DS    A               +8  - WORK  AREA   ANCHOR
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        REGISTER SAVE AREA OFFSETS:                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
RSABP    EQU   4
RSAFP    EQU   8
RSA14    EQU   12
RSA15    EQU   16
RSA0     EQU   20
RSA1     EQU   24
RSA2     EQU   28
*
GVBRD35  RMODE ANY
GVBRD35  AMODE 31
GVBRD35  CSECT
         J     CODE
RD35EYE  GVBEYE GVBRD35
*
CODE     STM   R14,R12,RSA14(R13) SAVE  CALLER'S  REGISTERS
*
         LA    R11,0(,R15)        SET   PROGRAM   BASE REGISTERS
         USING GVBRD35,R11
*
         LR    R8,R1              SAVE  PARAMETER DATA ADDRESS
         USING PARMLIST,R8
                        SPACE 3
         LR    R10,R13            SAVE  CALLER'S  RSA     ADDRESS
         L     R13,SORTWORK       ASSUME   NOT    FIRST   TIME
         USING WORKAREA,R13
*
         L     R0,SORTPREC        LOAD  PREVIOUS  OUTPUT  RECORD   ADDR
         LTR   R0,R0              PRESENT  ???
         BP    CHAIN              YES - BYPASS    FIRST   TIME    LOGIC
*
***********************************************************************
*  ALLOCATE "GVBRD35" WORKAREA IF NOT ALREADY ALLOCATED               *
***********************************************************************
         LHI   R0,WORKLEN         LOAD  WORK AREA SIZE
         GETMAIN RU,LV=(0),LOC=(BELOW)
         LR    R13,R1
         ST    R13,SORTWORK       SAVE  WORK AREA ADDRESS
*
         LR    R0,R13             ZERO  WORK AREA
         LA    R1,WORKLEN
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
         ST    R13,RSAFP(,R10)    SET   FORWARD  POINTER IN OLD
         ST    R10,RSABP(,R13)    SET   BACKWARD POINTER IN NEW
*
         BAS   R10,INITWORK       INITIALIZE WORK AREA
*
         B     SORTE35
                     EJECT
***********************************************************************
*  CHAIN REGISTER SAVE AREAS TOGETHER                                 *
***********************************************************************
CHAIN    ST    R13,RSAFP(,R10)    SET   FORWARD  POINTER IN OLD
         ST    R10,RSABP(,R13)    SET   BACKWARD POINTER IN NEW
*
*        LA    R9,WORKLEN-1(,R7)
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=102,STORAGE=((R7),(R9))
*
                     SPACE 3
***********************************************************************
*        E 3 5   S O R T   E X I T   L O G I C                        *
***********************************************************************
SORTE35  L     R1,SORTIREC        LOAD  INPUT RECORD  ADDRESS
         LTR   R1,R1              END-OF-FILE ???
         BRP   SORTCOPY           NO  -  ADD  PREFIX  TO  RECORD
*
***********************************************************************
*  UPDATE "REH" WITH FINAL RECORD COUNT AT END-OF-FILE                *
***********************************************************************
         LA    R2,HDRFILE
         USING IHADCB,R2
*
         L     R3,HDRRECA
         USING TBLHEADR,R3
*
         CVB   R0,SORTCNT
         ST    R0,TBRECCNT
*
         PUTX  (R2)
*
         CLOSE (R2),MODE=31,MF=(E,OPENPARM)
*
         L     R13,RSABP(,R13)    RESTORE  R13
         LM    R14,R12,RSA14(R13) RESTORE  R14 - R12
         LHI   R15,8              RETURN  (DO NOT CALL AGAIN)
         BR    R14
*
         DROP  R2
         DROP  R3
*
***********************************************************************
*  COPY "SORTOUT" RECORD TO OUTPUT AREA WITH PREFIX ADDED             *
***********************************************************************
SORTCOPY L     R14,SORTOREC       LOAD  TARGET RECORD ADDRESS
*
         TM    SORTRECF,X'80'     FIXED/UNDEFINED ???
         BRNO  SORTVAR            NO  - BRANCH
*
         LH    R15,SORTRECL       ASSUME FIXED LENGTH RECORD
         LHI   R0,8+8
         SR    R15,R0
         B     SORTPREF
*
SORTVAR  LH    R15,0(,R1)         LOAD  LENGTH FROM   RDW
         LA    R0,8+8(,R15)
         STH   R0,0(,R14)
         XC    2(2,R14),2(R14)
         AHI   R14,4
         S     R15,F4
         AHI   R1,4
*
SORTPREF XC    0(8+8,R14),0(R14)  ZERO  PREFIX AREA
         AHI   R14,8+8
*
         LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO MOVE
         SRL   R0,8
         BRC   15,SORTCEND
SORTCMVC MVC   0(0,R14),0(R1)     * * * * E X E C U T E D * * * *
SORTCMLP MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
SORTCEND BRCT  R0,SORTCMLP
         EX    R15,SORTCMVC       MOVE RECORD TO WORK   AREA
*
         AP    SORTCNT,P001       INCREMENT  "SORTOUT"  COUNT
                        SPACE 5
***********************************************************************
*        E 3 5   R E T U R N   T O   S O R T                          *
***********************************************************************
         L     R1,SORTOREC             LOAD EXTENDED  RECORD   ADDRESS
         L     R13,RSABP(,R13)         RESTORE SORT'S REGISTER CONTENTS
         ST    R1,RSA1(,R13)
         LM    R14,R12,RSA14(R13)
         SR    R15,R15                 SET  RC=0  (CHANGE RECORD)
         BSM   0,R14
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        I N I T I A L I Z E   W O R K   A R E A                      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
INITWORK ZAP   SORTCNT,P000           INITIALIZE SORT OUT COUNT
*
         MVC   OPENPARM,OPENINIT      INITIALIZE PARAMETER AREA
*
         LA    R2,SORTOUT
         USING IHADCB,R2
*
         L     R14,SORTDCBA                 COPY "SORTOUT"  DCB
         MVC   0(SORTDCBL,R2),0(R14)
*
         L     R14,REHDCBA                  COPY "MR95REH"  DCB
         MVC   HDRFILE(REHDCBL),0(R14)
         LA    R0,HDRFILE+REHDCBE-REHDCB    SET  DCBE ADDRESS IN DCB
         ST    R0,HDRFILE+DCBDCBE-IHADCB
*
         LA    R0,JFCB                INITIALIZE  DCB EXIT  LIST
         ST    R0,JFCBEXIT
         MVI   JFCBEXIT,X'87'         JFCB  IS   LAST ENTRY
         LA    R0,JFCBEXIT            COPY  EXIT LIST ADDRESS
         ST    R0,DCBEXLST
*
*        RDJFCB ((R2))                READ  JOB-FILE-CONTROL-BLOCK
         ST    R2,JFCBPARM
         MVI   JFCBPARM,X'80'
         LA    R1,JFCBPARM
         SVC   64
*
         LTR   R15,R15                SUCCESSFUL ???
         BNZ   OPENERR
*
         LA    R14,JFCB               LOAD  JFCB  ADDRESS
         USING JFCBAR,R14
*
         MVC   SORTRECF,JFCRECFM      SAVE  RECFM FROM JCL
*
         LH    R0,JFCLRECL            LOAD  LRECL FROM JCL
         STH   R0,SORTRECL
         GETMAIN RU,LV=(0),LOC=(ANY)
         ST    R1,SORTOREC
*
***********************************************************************
*  OPEN TEMPLATE "REH" FILE                                           *
***********************************************************************
INITREH  LA    R2,HDRFILE
         USING IHADCB,R2
*
         OPEN  ((2),(UPDAT)),MODE=31,MF=(E,OPENPARM)
         TM    48(R2),X'10'
         BO    *+6
         DC    H'0'
*
         GET   (2)
         ST    R1,HDRRECA
*
         BR    R10
*
OPENERR  L     R13,RSABP(,R13)    RESTORE  R13
         LM    R14,R12,RSA14(R13) RESTORE  R14 - R12
         LHI   R15,12             RETURN  (ABORT)
         BR    R14
*
         DROP  R2
         DROP  R14
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
P000     DC    PL2'00'
P001     DC    PL2'01'
F4       DC    F'4'
*
SORTDCBA DC    A(SORTDCB)
REHDCBA  DC    A(REHDCB)
*
OPENINIT DC    XL8'8000000000000000'
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D A T A   C O N T R O L   B L O C K S                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
SNAPDCB  DCB   DSORG=PS,DDNAME=SNAPDATA,MACRF=(W),                     X
               RECFM=VBA,LRECL=125,BLKSIZE=1632
*
REHDCB   DCB   DSORG=PS,DDNAME=MR95REH,MACRF=(GL,PL),                  X
               DCBE=REHDCBE
REHDCBE  DCBE  RMODE31=BUFF,EODAD=OPENERR
REHDCBL  EQU   *-REHDCB
                        SPACE 3
SORTDCB  DCB   DSORG=PS,DDNAME=SORTOUT,MACRF=(W)
SORTDCBL EQU   *-SORTDCB
                        EJECT
         PRINT NOGEN
         DCBD  DSORG=PS
                        SPACE 3
JFCBAR   DSECT
         IEFJFCBN LIST=YES
*
                        SPACE 3
         END
