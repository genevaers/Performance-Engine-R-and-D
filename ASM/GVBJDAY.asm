**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2005, 2021.
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
         TITLE 'GVBJDAY - CONVERT GREGORIAN DATE TO SERIAL DAY'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  GVBJDAY  - COMPUTE SERIAL DAY                                      *
*                                                                     *
*  The Julian date (JD) is a continuous count of days from 1 January  *
*  4713 BC.                                                           *
*                                                                     *
*  This program uses an algorithm described in (case-sensitive):      *
*                                                                     *
*          https://aa.usno.navy.mil/faq/JD_formula                    *
*                                                                     *
*  Refer to the assembler copybook GVBMJDAY or the COBOL copybook     *
*  GVBCJDAY for calling parameters.                                   *
*                                                                     *
*                                                                     *
*  RETURN CODES(R15):                                                 *
*                                                                     *
*        0  - SUCCESSFUL                                              *
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
*        R15 - TEMPORARY WORK     REGISTER                            *
*            - RETURN    CODE                                         *
*                                                                     *
*        R14 - RETURN    ADDRESS                                      *
*                                                                     *
*        R13 - REGISTER  SAVE     AREA    ADDRESS (CALLER PROVIDED)   *
*                                                                     *
*        R12 - WORK      AREA     BASE    ADDRESS                     *
*                                                                     *
*        R11 - PROGRAM   BASE     REGISTER                            *
*                                                                     *
*        R10 - PARAMETER LIST     ADDRESS                             *
*                                                                     *
*        R9  - EVEN/ODD  REGISTER PAIR   (3RD SUB-EXPRESSION)         *
*        R8  -                                                        *
*                                                                     *
*        R7  - EVEN/ODD  REGISTER PAIR   (2ND SUB-EXPRESSION)         *
*        R6  -                                                        *
*                                                                     *
*        R5  - EVEN/ODD  REGISTER PAIR   (1ST SUB-EXPRESSION)         *
*        R4  -                                                        *
*                                                                     *
*        R3  - "DD"                                                   *
*        R2  - "MM"                                                   *
*        R1  - "CCYY"                                                 *
*            - PARAMETER LIST    ADDRESS             (UPON ENTRY)     *
*                                                                     *
*        R0  - TEMPORARY WORK    REGISTER                             *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*       "GVBJDAY" - W O R K A R E A   D E F I N I T I O N             *
*                                                                     *
*       (NOTE: CANNOT BE EXPANDED BECAUSE IN REGISTER SAVE AREA)      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
WORKAREA DSECT
*
DBLWORK  DS    D                  DOUBLEWORD   WORK AREA
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P A R A M E T E R   L I S T   D E F I N I T I O N            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
PARMLIST DSECT
*
PLDATE1  DS    A                  INPUT  DATE - 1     (CCYYDDD)
PLSDAY   DS    A                  SERIAL DAY          (FULLWORD)
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
RSA14    EQU   12
RSA2     EQU   28
*
                        EJECT
         PRINT GEN
*
GVBJDAY  RMODE ANY
GVBJDAY  AMODE 31
GVBJDAY  CSECT
         J     CODE
JDAY     GVBEYE GVBJDAY
*
CODE     STM   R14,R12,RSA14(R13) SAVE  CALLER'S  REGISTERS
*
         LA    R11,0(,R15)        SET   PROGRAM   BASE    REGISTERS
         USING GVBJDAY,R11
*
         LR    R10,R1             LOAD  PARAMETER LIST    ADDRESS
         USING PARMLIST,R10
*
         LA    R12,RSA14+7(,R13)  LOAD  WORK AREA ADDR WITHIN RSA
         SRL   R12,3                  (ROUNDED TO DOUBLEWORD)
         SLL   R12,3
         USING WORKAREA,R12
*
***********************************************************************
*  CONVERT "CCYY", "MM", "DD" TO BINARY                               *
***********************************************************************
         L     R15,PLDATE1        CONVERT DATE-1
*
         PACK  DBLWORK,0(4,R15)
         CVB   R1,DBLWORK         "CCYY"
*
         PACK  DBLWORK,4(2,R15)
         CVB   R2,DBLWORK         "MM"
*
         PACK  DBLWORK,6(2,R15)
         CVB   R3,DBLWORK         "DD"
*
***********************************************************************
*  COMPUTE (MM-14)/12                                                 *
***********************************************************************
         LR    R4,R2
         AHI   R4,-14
         SRDA  R4,32
         D     R4,F12
*
***********************************************************************
*  COMPUTE 3RD SUB-EXPRESSION                                         *
***********************************************************************
         LR    R8,R5
         AHI   R8,4900
         AR    R8,R1
         SRDA  R8,32
         D     R8,F100
         M     R8,F3
         SRDA  R8,2               (3*((CCYY+4900+(MM-14)/12)/100)/4)
*
***********************************************************************
*  COMPUTE 2ND SUB-EXPRESSION                                         *
***********************************************************************
         LR    R7,R5
         M     R6,F12
         LR    R6,R2
         AHI   R6,-2
         SR    R6,R7
         LHI   R7,367
         MR    R6,R6
         D     R6,F12             (367*(MM-2-((MM-14)/12)*12)/12)
*
***********************************************************************
*  COMPUTE 1ST SUB-EXPRESSION                                         *
***********************************************************************
         AHI   R5,4800
         AR    R5,R1
         M     R4,F1461
         SRDA  R4,2               (1461*(CCYY+4800+(MM-14)/12)/4)
*
***********************************************************************
*  COMBINE SUB-EXPRESSIONS                                            *
***********************************************************************
         AHI   R3,-32075
         AR    R3,R5
         AR    R3,R7
         SR    R3,R9              DD-32075+SUB1+SUB2-SUB3
*
***********************************************************************
*  RETURN SERIAL DAY                                                  *
***********************************************************************
         L     R15,PLSDAY
         ST    R3,0(,R15)
*
***********************************************************************
*  RETURN TO CALLER                                                   *
***********************************************************************
         SR    R15,R15            ZERO  RETURN CODE
*
         LM    R2,R12,RSA2(R13)   RESTORE REGISTERS R2 - R12
         bsm   0,r14              return in the correct amode
                        SPACE 3
         DS   0D
F3       DC    F'3'
F12      DC    F'12'
F100     DC    F'100'
F1461    DC    F'1461'
*
         LTORG
*
*
         END
