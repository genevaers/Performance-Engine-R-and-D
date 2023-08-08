         TITLE 'GVBRD15 - BUILD GENEVA REFERENCE DATA FILE'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2006, 2021
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
*  GVBRD15  - CONVERTS FIXED LENGTH INPUT RECORDS TO VARIABLE LENGTH  *
*             FORMAT BY ADDING AN "RDW" (VARIABLE LENGTH INPUT        *
*             RECORDS ARE LEFT UNCHANGED)                             *
*                                                                     *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            0  - SUCCESSFUL                                          *
*            8  - DON'T  CALL AGAIN                                   *
*           12  - INSERT A   RECORD                                   *
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
*        R3  -                                                        *
*        R2  - DCB ADDRESS                                            *
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
*       S O R T   E X I T   P A R A M E T E R   L I S T               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
PARMLIST DSECT
*
SORTIREC DS    A               +0  - INPUT    RECORD ADDRESS
SORTWORK DS    A               +4  - WORK  AREA   ANCHOR
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
*
GVBRD15  RMODE 24
GVBRD15  AMODE 31
GVBRD15  CSECT
         J     CODE
RD15EYE  GVBEYE GVBRD15
*
CODE     STM   R14,R12,RSA14(R13) SAVE  CALLER'S  REGISTERS
*
         LA    R11,0(,R15)        SET   PROGRAM   BASE REGISTERS
         USING GVBRD15,R11
*
         LR    R8,R1              SAVE  PARAMETER DATA ADDRESS
         USING PARMLIST,R8
                        SPACE 3
         LR    R10,R13            SAVE  CALLER'S  RSA     ADDRESS
         LA    R13,SAVEAREA       ASSUME   NOT    FIRST   TIME
*
         ST    R13,RSAFP(,R10)    SET   FORWARD  POINTER IN OLD
         ST    R10,RSABP(,R13)    SET   BACKWARD POINTER IN NEW
*
         CLI   FIRSTIME,C'Y'      FIRST CALL ???
         BRNE  SORTE15            YES - BYPASS    FIRST   TIME    LOGIC
         MVI   FIRSTIME,C'N'
*
         BRAS  R10,INITWORK       INITIALIZE WORK AREA
                     EJECT
***********************************************************************
*        E 1 5   S O R T   E X I T   L O G I C                        *
***********************************************************************
SORTE15  LA    R2,SORTDCB         LOAD  INPUT DCB     ADDRESS
         USING IHADCB,R2
*
         GET   (2)
*
***********************************************************************
*  COPY "SORTIN" RECORD TO OUTPUT AREA WITH "RDW" ADDED IF NEEDED     *
***********************************************************************
         L     R14,SORTOREC       LOAD  TARGET RECORD ADDRESS
*
         TM    DCBRECFM,X'80'     FIXED/UNDEFINED ???
         BRNO  SORTVAR            NO  - BRANCH
*
         LH    R15,SORTRECL       ASSUME FIXED LENGTH RECORD
         STH   R15,0(,R14)
         XC    2(2,R14),2(R14)
         AHI   R14,4
         AHI   R15,-4
         BRU   SORTPREF
*
SORTVAR  LH    R15,0(,R1)         LOAD  LENGTH FROM   RDW
         STH   R15,0(,R14)
         XC    2(2,R14),2(R14)
         AHI   R14,4
         AHI   R15,-4
         AHI   R1,4
*
SORTPREF LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO MOVE
         SRL   R0,8
         BRU   SORTCEND
SORTCMVC MVC   0(0,R14),0(R1)     * * * * E X E C U T E D * * * *
SORTCMLP MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
SORTCEND BRCT  R0,SORTCMLP
         EX    R15,SORTCMVC       MOVE RECORD TO WORK   AREA
*
         AP    SORTCNT,P001       INCREMENT  "SORTIN"   COUNT
                        SPACE 5
***********************************************************************
*        E 1 5   R E T U R N   T O   S O R T                          *
***********************************************************************
         L     R1,SORTOREC             LOAD EXTENDED  RECORD   ADDRESS
         L     R13,RSABP(,R13)         RESTORE SORT'S REGISTER CONTENTS
         ST    R1,RSA1(,R13)
         LM    R14,R12,RSA14(R13)
         LHI   R15,12                  SET  RC=12 (INSERT RECORD)
         BR    R14
*
***********************************************************************
*  CLOSE INPUT FILE                                                   *
***********************************************************************
SORTEOF  LA    R2,SORTDCB
         CLOSE ((R2)),MODE=31
*
         L     R13,RSABP(,R13)    RESTORE  R13
         LM    R14,R12,RSA14(R13) RESTORE  R14 - R12
         LHI   R15,8              RETURN  (DO NOT CALL AGAIN)
         BR    R14
*
         DROP  R2
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        I N I T I A L I Z E   W O R K   A R E A                      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
INITWORK LA    R2,SORTDCB
         USING IHADCB,R2
*
         OPEN  ((2),(INPUT)),MODE=31
         TM    48(R2),X'10'  SUCCESSFUL ???
         BRNO  OPENERR
*
         LH    R0,DCBLRECL            LOAD  LRECL FROM JCL
         TM    DCBRECFM,X'80'         FIXED/UNDEFINED  ???
         BRNO  *+8                    NO  - ASSUME ALREADY VARIABLE
         AHI   R0,4                   ADD   RDW    LENGTH
         STH   R0,SORTRECL
         GETMAIN RU,LV=(0),LOC=(ANY)
         ST    R1,SORTOREC
*
         BR    R10
*
OPENERR  L     R13,RSABP(,R13)    RESTORE  R13
         LM    R14,R12,RSA14(R13) RESTORE  R14 - R12
         LHI   R15,16             RETURN  (ABORT)
         BR    R14
*
         DROP  R2
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N S T A N T S   A N D   V A R I A B L E S                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         DS   0D
SAVEAREA DC  18A(0)
*
SORTCNT  DC    PL8'0'
*
SORTOREC DC    A(0)
*
SORTRECL DC    H'0'
*
P001     DC    PL2'01'
*
FIRSTIME DC    CL1'Y'
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D A T A   C O N T R O L   B L O C K S                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
SORTDCB  DCB   DSORG=PS,DDNAME=MR95RED,MACRF=(GL),                     X
               DCBE=SORTDCBE
SORTDCBE DCBE  RMODE31=BUFF,EODAD=SORTEOF
SORTDCBL EQU   *-SORTDCB
                        SPACE 3
         PRINT NOGEN
         DCBD  DSORG=PS
                        SPACE 3
*
         END
