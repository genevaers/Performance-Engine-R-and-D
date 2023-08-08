         TITLE 'GVBXLENV - GET ENVIRONMENT INFORMATION'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2009, 2021.
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
*      - THIS MODULE RETURNS THE ADDRESS OF THE SAFR ENVIRONMENT      *
*        INFORMATION                                                  *
*                                                                     *
*                                                                     *
*  SAFR MODULES USED      : NONE                                      *
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
*            0  - SUCCESSFUL                                          *
*            4  -                                                     *
*            8  -                                                     *
*           12  -                                                     *
*           16  -                                                     *
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
*        R13 - CALLER  SAVE AREA ADDRESS                              *
*        R12 -                                                        *
*        R11 -                                                        *
*        R10 -                                                        *
*        R9  -                                                        *
*        R8  -                                                        *
*        R7  -                                                        *
*        R6  -                                                        *
*        R5  -                                                        *
*        R4  -                                                        *
*        R3  -                                                        *
*        R2  -                                                        *
*                                                                     *
*        R1  - PARAMETER LIST    ADDRESS             (UPON ENTRY)     *
*                                                                     *
*        R0  - TEMPORARY WORK    REGISTER                             *
*                                                                     *
***********************************************************************
*                                                                     *
***********************************************************************
                        EJECT
         COPY GVBX95PA
                        EJECT
***********************************************************************
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
***********************************************************************
         YREGS
*
***********************************************************************
*                                                                     *
*        REGISTER SAVE AREA OFFSETS:                                  *
*                                                                     *
***********************************************************************
RSABP    EQU   4
RSAFP    EQU   8
RSA14    EQU   12
RSA15    EQU   16
RSA0     EQU   20
RSA1     EQU   24
*
                        EJECT
         PRINT GEN
*
GVBXLENV RMODE ANY
GVBXLENV AMODE 31
GVBXLENV CSECT
         J     CODE
XLENEYE  GVBEYE GVBXLENV
*
CODE     STM   R14,R12,RSA14(R13) SAVE  CALLER'S  REGISTERS
*
         USING GENPARM,R1
*
         LLGT  R0,GPENVA          LOAD ENV  INFO POINTER ADDRESS
         L     R14,GPBLOCKA       LOAD RESULT    POINTER ADDRESS
         STG   R0,0(,R14)
*
***********************************************************************
*  RETURN TO CALLER (SAFR)                                            *
***********************************************************************
         SR    R15,R15            ZERO RETURN CODE
         L     R14,GPRTNCA        LOAD RETURN CODE  ADDRESS
         ST    R15,0(,R14)
*
         L     R14,RSA14(,R13)    RESTORE REGISTER  R14
         LM    R0,R12,RSA0(R13)   RESTORE REGISTERS R0 - R12
         BSM   0,R14              RETURN
*
         END
