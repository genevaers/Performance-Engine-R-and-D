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
***********************************************************************
         TITLE 'GVBGDAT - CONVERT SERIAL DAY TO GREGORIAN DATE'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  GVBGDAT  - COMPUTE GREGORIAN DATE                                  *
*                                                                     *
*  The Julian date (JD) is a continuous count of days from 1 January  *
*  4713 BC.                                                           *
*                                                                     *
*  This program uses an algorithm described in (case-sensitive):      *
*                                                                     *
*      https://aa.usno.navy.mil/faq/JD_formula                        *
*                                                                     *
*  Refer to the assembler copybook GVBMGDAT or the COBOL copybook     *
*  GVBCGDAT for calling parameters.                                   *
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
*       "GVBGDAT" - W O R K A R E A   D E F I N I T I O N             *
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
PLDATE   DS    A                  RESULT DATE - NUMERIC (CCYYMMDD)
PLSDAY   DS    A                  SERIAL DAY  - BINARY  (FULLWORD)
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
GVBGDAT  RMODE ANY
GVBGDAT  AMODE 31
GVBGDAT  CSECT
         J     CODE
GDATEYE  GVBEYE GVBGDAT
*
CODE     STM   R14,R12,RSA14(R13) SAVE  CALLER'S  REGISTERS
*
         LA    R11,0(,R15)        SET   PROGRAM   BASE    REGISTERS
         USING GVBGDAT,R11
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
*                                                                     *
***********************************************************************
         L     R1,PLSDAY          LOAD JULIAN DAY
         L     R1,0(,R1)
*
         A     R1,F68569          L=JD+68569
*
         LR    R2,R1                  L
         SRDA  R2,32
         SLDA  R2,2                 4*L
         D     R2,F146097         N=4*L/146097
         LR    R7,R3              SAVE "N"
*
         M     R2,F146097              146097*N
         AHI   R3,3                    146097*N+3
         SRDA  R2,2                   (146097*N+3)/4
         SR    R1,R3              L=L-(146097*N+3)/4
*
         LA    R3,1(,R1)                  L+1
         M     R2,F4000             4000*(L+1)
         D     R2,F1461001        I=4000*(L+1)/1461001
         LR    R15,R3             SAVE "I"
*
         M     R2,F1461               1461*I
         SRDA  R2,2                   1461*I/4
         SR    R1,R3                L-1461*I/4
         AHI   R1,31              L=L-1461*I/4+31
*
         LR    R3,R1                   L
         M     R2,F80               80*L
         D     R2,F2447           J=80*L/2447
         LR    R0,R3              SAVE "J"
*
         LR    R5,R3                       J
         M     R4,F2447               2447*J
         D     R4,F80                 2447*J/80
         SR    R1,R5              K=L-2447*J/80    (R1=DAY)
*
         SR    R2,R2
         D     R2,F11             L=J/11
*
         LR    R5,R3                       L
         LHI   R4,12                    12
         MR    R4,R4                    12*L
         AHI   R0,2                 J+2
         SR    R0,R5              J=J+2-12*L       (R0=MONTH)
*
         AHI   R7,-49                    N-49
         LHI   R6,100               100
         MR    R6,R6                100*(N-49)
         AR    R7,R15               100*(N-49)+I
         AR    R7,R3              I=100*(N-49)+I+L (R7=YEAR)
*
*
***********************************************************************
*  RETURN GREGORIAN DATE                                              *
***********************************************************************
         L     R15,PLDATE
         CVD   R7,DBLWORK                           YEAR
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  0(4,R15),DBLWORK
         CVD   R0,DBLWORK                           MONTH
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  4(2,R15),DBLWORK
         CVD   R1,DBLWORK                           DAY
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  6(2,R15),DBLWORK
*
***********************************************************************
*  RETURN TO CALLER                                                   *
***********************************************************************
         SR    R15,R15            ZERO  RETURN CODE
*
         LM    R2,R12,RSA2(R13)   RESTORE REGISTERS R2 - R12
         Bsm   0,r14              return in the correct amode
                        SPACE 3
         DS   0D
F11      DC    F'11'
F80      DC    F'80'
F1461    DC    F'1461'
F2447    DC    F'2447'
F4000    DC    F'4000'
F68569   DC    F'68569'
F146097  DC    F'146097'
F1461001 DC    F'1461001'
*
         LTORG
*
         END
