         TITLE 'GVBUR05 - GETMAIN'
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
*                                                                     *
*  GVBUR05 - ALLOCATES MAIN MEMORY ABOVE THE 16M LINE (GETMAIN)       *
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
         PRINT NOGEN
*
GVBUR05  RMODE ANY
GVBUR05  AMODE 31
GVBUR05  CSECT
         PRINT OFF
         COPY  ASMMSP
LEAVE    OPSYN ASM_LEAVE
         ASMMREL ON
         PRINT ON
         SYSSTATE ARCHLVL=2

         J     CODE
UR05EYE  GVBEYE GVBUR05
*
code     stm   r14,r4,rsa14(r13)  save register r14 - r4
*
         lr    r4,r15
         using gvbur05,r4

         l     r2,0(,r1)          load pointer address from parm list
         l     r1,4(,r1)          load length  address from parm list
         l     r0,0(,r1)          load length
         lr    r3,r0                and copy for later

         storage OBTAIN,          Allocate                             +
               LENGTH=(0),         this amount                         +
               LOC=(ANY),           located anywhere                   +
               CHECKZERO=YES         and test for clearing

         st    r1,0(,r2)          save address

         if    (cgij,r15,ne,x'14')  X'14' signals the storage is clean

           lr    r0,r1            copy the address to r0
           lr    r1,r3            reload the length
           xr    r14,r14          clear r14
           xr    r15,r15
           mvcl  r0,r14           clear the area

         endif

         lm    r14,r4,rsa14(r13)  restore register r14 - r4
         sr    r15,r15            zero return code
         bsm   0,r14              return

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
         end
