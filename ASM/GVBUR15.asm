         TITLE 'GVBUR15 - SET ADDRESS POINTER'
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
*  GVBUR15 - SETS A POINTER VARIABLE TO THE ADDRESS OF A DATA AREA    *
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
         EJECT
         PRINT NOGEN
*
GVBUR15  RMODE ANY
GVBUR15  AMODE 31
GVBUR15  CSECT
         J     CODE
UR15EYE  GVBEYE GVBUR15
*
CODE     L     R15,0(,R1)         LOAD POINTER ADDRESS FROM PARM LIST
         L     R0,4(,R1)          LOAD AREA    ADDRESS FROM PARM LIST
         ST    R0,0(,R15)         SET  POINTER
         NI    0(R15),X'7F'       TURN OFF HIGH ORDER BIT IF ON
*
RETURN   SR    R15,R15            ZERO RETURN  CODE
         BSM   0,R14
*
         END
