         TITLE 'GVBUR10 - FREEMAIN'
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  GVBUR10 - RELEASES MAIN MEMORY (FREEMAIN)                          *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            0  - SUCCESSFUL                                          *
*                                                                     *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - TEMPORARY WORK REGISTER                                *
*            - RETURN    CODE                                         *
*                                                                     *
*        R14 - RETURN    ADDRESS                                      *
*                                                                     *
*        R13 - REGISTER  SAVE AREA  ADDRESS  (CALLER PROVIDED)        *
*                                                                     *
*                                                                     *
*        R1  - PARAMETER LIST ADDRESS                                 *
*            - ALLOCATED AREA ADDRESS                                 *
*                                                                     *
*        R0  - TEMPORARY WORK REGISTER                                *
*            - ALLOCATED AREA LENGTH                                  *
*                                                                     *
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
*
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
         EJECT
         PRINT NOGEN
*
GVBUR10  RMODE ANY
GVBUR10  AMODE 31
GVBUR10  CSECT
         USING GVBUR10,R15
         J     CODE
UR10EYE  GVBEYE GVBUR10
*
CODE     ST    R2,RSA2(,R13)      SAVE        REGISTER
*
         LR    R2,R1              LOAD PARAMETER  LIST ADDRESS
         L     R1,0(,R1)          LOAD POINTER ADDRESS FROM PARM LIST
         L     R1,0(,R1)          LOAD AREA    ADDRESS FROM POINTER
         L     R2,4(,R2)          LOAD LENGTH  ADDRESS FROM PARM LIST
         L     R0,0(,R2)          LOAD LENGTH
*
         FREEMAIN RU,LV=(0),A=(1) RELEASE      MEMORY
*
RETURN   L     R2,RSA2(,R13)      RESTORE      REGISTER
         SR    R15,R15            ZERO RETURN  CODE
         BSM   0,R14              RETURN
*
         END
