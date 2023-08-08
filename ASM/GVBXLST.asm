         TITLE 'GVBXLST - CONCATENATE STRINGS'
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
*      - THIS MODULE CONCATENATES UP TO 11 STRINGS CONTAINED IN THE   *
*        LOOK-UP KEY AREA                                             *
*                                                                     *
*      - THE LENGTHS OF THE 11 STRINGS ARE CONTAINED IN 11 POSITIONAL *
*        HALFWORDS AT THE BEGINNING OF THE LOOK-UP KEY                *
*                                                                     *
*      - A NEGATIVE LENGTH INDICATES "AS IS" WITH NO TRAILING SPACE   *
*        TRIMMING                                                     *
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
*            0  - SUCCESSFUL                                          *
*            4  -                                                     *
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
*        R15 - TEMPORARY  WORK   REGISTER                             *
*            - RETURN     CODE                                        *
*                                                                     *
*        R14 - TEMPORARY  WORK   REGISTER                             *
*            - RETURN     ADDR                                        *
*                                                                     *
*        R13 - WORK AREA  ADDRESS                                     *
*                                                                     *
*        R12 -                                                        *
*        R11 - PROGRAM    BASE   REGISTER                             *
*                                                                     *
*        R10 - SUBROUTINE CALL   RETURN   ADDRESS (1ST LEVEL)         *
*                                                                     *
*        R9  - PARAMETER  LIST   ADDRESS                              *
*                                                                     *
*        R8  - LOOK-UP    KEY    ADDRESS                              *
*                                                                     *
*        R7  - CURRENT    LENGTH ADDRESS                              *
*        R6  - CURRENT    INPUT  ADDRESS                              *
*        R5  - CURRENT    OUTPUT ADDRESS                              *
*                                                                     *
*        R4  - PREVIOUS NEGATIVE LENGTH  OUTPUT ADDRESS               *
*        R3  - PREVIOUS NEGATIVE LENGTH  (POSITIVE)                   *
*                                                                     *
*        R2  -                                                        *
*                                                                     *
*        R1  - TEMPORARY  WORK   REGISTER                             *
*            - PARAMETER  LIST   ADDRESS             (UPON ENTRY)     *
*                                                                     *
*        R0  - TEMPORARY  WORK   REGISTER                             *
*                                                                     *
***********************************************************************
                        EJECT
***********************************************************************
*                                                                     *
*       "GVBXLST" - W O R K A R E A   D E F I N I T I O N             *
*                                                                     *
***********************************************************************
*
WORKAREA DSECT
EYEBALL  DS    CL8
*
         IHASAVER DSECT=NO,SAVER=NO,SAVF4SA=YES,TITLE=NO

DBLWORK  DS    D
*
OUTKEY   DS    CL252
OUTREC   DS    CL256              RETURNED LOOK-UP  "LR"
*
WORKLEN  EQU   *-WORKAREA
*                                                                     *
***********************************************************************
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        GENEVA PROVIDED PARAMETER LIST                               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         copy    gvbx95pa
*
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        GENEVA LOOK-UP KEY                                           *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
LKUPKEY  DSECT                 LOOK-UP  KEY
*
KYLRID   DS    FL04            LOGICAL  RECORD  ID
KYLENTBL DS  11HL02            STRING   LENGTHS
KYDATA   DS    CL230           STRING   DATA
                        EJECT
***********************************************************************
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
***********************************************************************
         YREGS
*
                        EJECT
         PRINT GEN
*
GVBXLST  RMODE 31
GVBXLST  AMODE 31
GVBXLST  CSECT
         J     start
XLSTEYE  GVBEYE GVBXLST
*
static   loctr
CODE     loctr
         org   *,256
         USING WORKAREA,R13
start    stmg  R14,R12,SAVF4SAG64RS14 CALLER'S  REGISTERS
*
         Llgtr R11,R15            SET   PROGRAM   BASE    REGISTER
         USING GVBXLST,R11
*
         Llgtr R9,R1              LOAD  PARAMETER LIST    ADDRESS
         USING GENPARM,R9
*
         Llgt  R8,GPKEYA          LOAD  LOOK-UP   KEY     ADDRESS
         USING LKUPKEY,R8
*
         LGR   R10,R13            SAVE  CALLER'S  RSA     ADDRESS
*
         Llgt  R2,GPWORKA         LOAD  WORK AREA POINTER ADDRESS
         L     R13,0(,R2)         LOAD  POINTER   VALUE
         LTR   R13,R13            ALLOCATED ???
         BRP   CHAIN              YES - BYPASS ALLOCATION
*
***********************************************************************
*  ALLOCATE "GVBXLST" WORKAREA IF NOT ALREADY ALLOCATED (PER THREAD)  *
***********************************************************************
WORKALLO STORAGE OBTAIN,LENGTH=WORKLEN,COND=NO,CHECKZERO=YES
         LR    R13,R1
         ST    R13,0(,R2)         SAVE  WORK AREA ADDRESS (POINTER)
*
         LR    R0,R13             ZERO  WORK AREA
         LHI   R1,WORKLEN
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
         MVC   EYEBALL,WORKEYEB
         STG   R13,savf4sanext-WORKAREA(0,r10) fwd POINTER IN OLD
         STG   R10,savf4saprev    SET   BACKWARD POINTER IN NEW
*
         L     R14,GPENVA         OPEN  PHASE ???
         USING GENENV,R14
*
         CLI   GPPHASE,C'O'
         BRNE  MAINLINE
*
         LHI   R15,8
         BRU   RETURN
*
         DROP  R14
*
                        EJECT
***********************************************************************
*  CHAIN REGISTER SAVE AREAS TOGETHER                                 *
*  CHECK FOR CHANGE IN EVENT RECORD ADDRESS                           *
***********************************************************************
CHAIN    DS    0H
         STG   R13,savf4sanext-WORKAREA(0,r10) fwd POINTER IN OLD
         STG   R10,savf4saprev    SET   BACKWARD POINTER IN NEW
*
***********************************************************************
*  COPY LOOK-UP KEY FIELDS TO OUTPUT "LR"                             *
***********************************************************************
MAINLINE L     R14,GPENVA         CHECK FOR   CLOSE PHASE
         USING GENENV,R14
*
         CLI   GPPHASE,C'C'
         BRE   RETURN0
         DROP  R14
*
         MVC   OUTKEY(11*L'KYLENTBL),KYLENTBL
*
         L     R14,GPBLOCKA       LOAD RESULT POINTER ADDRESS
         LA    R1,OUTKEY          address output version of key
         STG   R1,0(,R14)
*
* ?      LHI   R0,L'KYDATA
* ?      L     R14,GPBLKSIZ       LOAD RESULT LENGTH  ADDRESS
* ?      ST    R0,0(,R14)
*
***********************************************************************
*  Set up and initialize output buffer                               *
***********************************************************************
*
         MVI   OUTREC+0,C' '      BLANK OUT OUTPUT AREA
         MVC   OUTREC+1(L'OUTREC-1),OUTREC
*
         LA    R5,OUTREC          OUTPUT DATA ADDRESS
*
         LA    R6,KYDATA          INPUT DATA ADDRESS
         LA    R7,KYLENTBL        1ST SUB-STRING LEN ADDRESS
*
         XR    R4,R4              ZERO PREVIOUS NEGATIVE LEN SUBSTR ADR
         XR    R3,R3              ZERO PREVIOUS NEGATIVE LEN SUBSTR LEN
*
         LHI   R10,11             INITIALIZE LOOP  COUNTER
*
***********************************************************************
*  LOOP THROUGH SUB-STRING LENGTHS                                    *
***********************************************************************
STRLOOP  LH    R15,0(,R7)         LOAD SUB-STRING  LENGTH
         LTR   R15,R15
         BRP   TRIM
         BRZ   DONE
*
         LCR   R15,R15            COMPLIMENT NEGATIVE LENGTH
         BCTR  R15,0
         EX    R15,MVCR5R6
*
         LR    R4,R5
         LR    R3,R15
*
         LA    R6,1(R6,R15)       ADVANCE  INPUT   ADDRESS
         LA    R5,1(R5,R15)       ADVANCE  OUTPUT  ADDRESS
*
         BRU   STRNEXT
*
TRIM     DS    0H                 COPY NON BLANKS SUB-STRING
         XR    R1,R1
         BCTR  R15,0
         LA    R14,0(R15,R6)      Right most byte of source
         EX    R15,TRTRTRAIL      Look for last significant char
         LTR   R1,R1              Anything found ?
         JZ    TRIM01             Nothing found
         SR    R1,R6              Length to move -1
         EX    R1,MVCR5R6
         LA    R5,1(R1,R5)        Advance next position of target
         XR    R4,R4              RESET   PREVIOUS SEPARATOR
         J     TRIM02
TRIM01   EQU   *
         LTR   R4,R4              PREVIOUS NEGATIVE LEN SUB-STRING
         BRNP  TRIM02
*
         LR    R5,R4              OVERLAY PREVIOUS  SUB-STRING
         EX    R3,BLNKOUT          (ELIMINATE SEPARATOR WHEN)
*
TRIM02   EQU   *
         LA    R6,1(R15,r6)       Advance next position of source
*
STRNEXT  AHI   R7,2               ADVANCE TO NEXT  SUB-STRING LEN
         BRCT  R10,STRLOOP
*
DONE     EQU   *
***********************************************************************
*  RETURN TO CALLER (GENEVA)                                          *
***********************************************************************
RETURN0  SGR   R15,R15            ZERO RETURN CODE
*
RETURN   lg    r13,savf4saprev    RESTORE REGISTER  R13
*
         LLGT  R14,GPRTNCA        LOAD RETURN CODE  ADDRESS
         ST    R15,0(,R14)
*
         lg    r14,savf4sag64rs14 RESTORE REGISTER  R14
         lmg   r0,r12,savf4sag64rs0
         BSM   0,R14              RETURN
*
static   loctr
*
MVCR5R6  MVC   0(0,R5),0(R6)      * * * * *  E X E C U T E D  * * * * *
BLNKOUT  MVC   0(0,R5),SPACES     * * * * *  E X E   U T E D  * * * * *
TRTRTRAIL TRTR 0(0,R14),NOTSPACES * * * * *  E X E C U T E D  * * * * *
*
NOTSPACES DS   0XL256                                 And not x'00' !!
         DC    XL16'00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
*
         DROP  R8
         DROP  R9
         DROP  R13
*
                        EJECT
***********************************************************************
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
***********************************************************************
*
         DS   0D
*
WORKEYEB DC    CL8'XLSTWORK'
SPACES   DC    CL256' '
*
         LTORG
*
*
         END
