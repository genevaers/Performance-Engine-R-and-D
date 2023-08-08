        TITLE 'GVBXRA - ADABAS READ EXIT USING MULTIFETCH'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2023.
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
*  MODULE DESCRIPTION     : THIS MODULE WORKS WITHIN THE "GENEVA"     *
*                           ENVIRONMENT AND IS A READ EXIT.           *
*                                                                     *
*                         : THIS MODULE READS ADABAS DATA FOR THE     *
*                           EXAMPLE "EMPLOYEE" FILE AND IS CODED      *
*                           SPECIFICALLY FOR THIS PURPOSE.            *
*                                                                     *
*                           Version 8 link routine.                   *
*                           It uses the V8 APLX, ACBX and ABDs.       *
*                                                                     *
***********************************************************************
*                                                                     *
* EXAMPLE ADARUN CARD PARAMETERS TO INCLUDE IN GENEVA PASS            *
*                                                                     *
*       //DDCARD    DD   *                                            *
*       ADARUN PROG=RENTUSER,MODE=MULTI,SVC=245,DEVICE=3390,DBID=300  *
*       /*                                                            *
*                                                                     *
***********************************************************************
                        EJECT
***********************************************************************
*                                                                     *
*           MODULE RETURN CODES AND REGISTER DOCUMENTATION            *
*                                                                     *
***********************************************************************
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*          0   - SUCCESSFUL                                           *
*          4   -                                                      *
*          8   - END-OF-FILE                                          *
*         12   - DISABLE   VIEW                                       *
*         16   - ABEND     JOB                                        *
*                                                                     *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - TEMPORARY WORK REGISTER                                *
*            - RETURN    CODE                                         *
*                                                                     *
*        R14 - TEMPORARY WORK REGISTER                                *
*            - RETURN    ADDRESS                                      *
*                                                                     *
*        R13 - REGISTER  SAVE AREA    ADDRESS (GVBXRA  WORK AREA)     *
*                                                                     *
*        R12 - PROGRAM   BASE REGISTER                                *
*        R11 - Work register                                          *
*                                                                     *
*        R10 - INTERNAL  SUBROUTINE   RETURN   ADDRESS                *
*                                                                     *
*        R9  - PARAMETER LIST GENPARM ADDRESS                         *
*                                                                     *
*        R8  - WORK   REGISTER                                        *
*                                                                     *
*        R7  - CURRENT   EVENT FILE   RECORD  ADDRESS                 *
*        R6  - CURRENT   FILE  CONTROL BLOCK  ADDRESS                 *
*        R5  - CURRENT   OPEN  FILE     LIST  ENTRY                   *
*                                                                     *
*        R4  - STARTING  FILE  CONTROL BLOCK  ADDRESS                 *
*        R3  - STARTING  OPEN  FILE     LIST  ENTRY    (BREAK)        *
*                                                                     *
*        R2  - LOWEST    KEY   VALUE   EVENT  RECORD                  *
*        R1  - LOWEST    KEY   VALUE    FILE  AREA     ADDRESS        *
*            - PARAMETER LIST  ADDRESS                                *
*            - TEMPORARY WORK  REGISTER                               *
*                                                                     *
*        R0  - TEMPORARY WORK  REGISTER                               *
*                                                                     *
***********************************************************************
*
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        GENEVA PROVIDED PARAMETER LISTS                              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         COPY  GVBX95PA         GENEVA  PARAMETER LIST
*        copy  gvbhdr

         IHASAVER DSECT=YES,SAVER=YES,SAVF4SA=YES,SAVF5SA=YES,TITLE=NO
*
***********************************************************************
*                                                                     *
*                     W O R K   A R E A S                             *
*                                                                     *
***********************************************************************
*
         ADACBX DSECT=YES
*
         ADABDX TYPE=EQ
*
         ADABDX TYPE=FB
         ADABDX TYPE=RB
         ADABDX TYPE=SB
         ADABDX TYPE=VB
         ADABDX TYPE=IB
         ADABDX TYPE=MB
*
MBDSECT  DSECT
MBRECL   DS    F
MBRESP   DS    F
MBISN    DS    F
MBISNQ   DS    F
*
WORKAREA DSECT
         ds    Xl(SAVF4SA_LEN)
*
WKSAVSUB DS  18fd                 INTERNAL  SUBROUTINE  REG  SAVE  AREA
WKSAB    DS  18fd                 Sub savearea
WKSUBADA DS  18fd                 Savearea for calling ADABAS
*
WKMR95WA DS    A                 "GVBMR95"  WORK  AREA  ADDRESS
WKREG10  DS    A                 Preserve R10 (return address)
*
WKTIMWRK DS   0XL16
WKDBLWRK DS    D                  TEMPORARY DOUBLEWORD  WORK AREA
WKDBLWK2 DS    D                  TEMPORARY DOUBLEWORD  WORK AREA
WKDBLWK3 DS    D                  TEMPORARY DOUBLEWORD  WORK AREA
*
WKPLISTA DS    A                  CALLER    PROVIDED    PARAMETER  LIST
*
WKDDNAME DS    CL8                LOGICAL EVENT   FILE  DDNAME
WKVIEW#  DS    FL4                LOGICAL EVENT   FILE  VIEW#
WKGPLFID DS    FL4                LOGICAL EVENT   FILE  GPLFID
WKENVV   DS    CL32               ENVV
WKDATETIME DS  CL16               DATETIME
WKSTART  DS    CL32               START-UP DATA
*
WKFILCNT DS    F                  FILE    CONTROL BLOCK AREA
WKFILSIZ DS    F
WKFILBEG DS    A
WKFILMAX DS    A
WKFILELO DS    A                  OPEN FILE LIST ENTRY  FOR LOWEST KEY
*
*
WKCALL64 DS    AD                 64-bit Event Record address
*
WKCALLPL DS   0d                  LAST  EXIT CALL PARAMETER  LIST
WKCALLP1 DS    A                  ENVIRONMENT  INFO ADDR
WKCALLP2 DS    A                  EVENT  FILE  INFO ADDR
WKCALLP3 DS    A                  STARTUP      DATA ADDR
WKCALLP4 DS    A                  EVENT RECORD  PTR ADDR
WKCALLP5 DS    A                  EXTRACT    RECORD ADDR
WKCALLP6 DS    A                  LOOK-UP    KEY    ADDR
WKCALLP7 DS    A                  WORK AREA POINTER ADDR
WKCALLP8 DS    A                  RETURN    CODE    ADDR
WKCALLP9 DS    A                  RETURNED  REC PTR ADDR
WKCALLPA DS    A                  RETURNED  REC LEN ADDR
WKCALLEN EQU   *-WKCALLPL         RETURNED  REC LEN ADDR
*
REENTWK  DS    XL128              RE-ENTRANT PARAMETER   LIST
WKPRINT  DS    XL131           Print line
WKTRACE  DS    CL1             Tracing
         DS   0F
WKREENT  DS    XL256           Reentrant workarea
WKDBLWK  DS    XL08            Double work workarea
*
WKOUTREC DS    A                  CURRENT   OUTPUT     RECORD
*
WKTOKNRC DS    A                  NAME/TOKEN  SERVICES RETURN CODE
WKTOKNAM DS    XL16               TOKEN NAME
WKTOKN   DS   0XL16               TOKEN VALUE
WKTOKNGB DS    A                  GLOBAL AREA
         DS    A
         DS    A
         DS    A
*
WKEOF    DS    CL1
WKREAD1  DS    XL1
WKTHRDNO DS    H
*
         DS    0F
WKGLOBA  DS    F                  ADDR GLOBAL AREA
WKPARTS# DS    F
WKRECCNT DS    F                  NUMBER OF RECORDS READ
WKRECBUF DS    F                  NUMBER OF RECORDS IN BUFFER
WKBUFRET DS    F                  NUMBER OF BUFFERS RETURNED
WKRXITRC DS    F                  LAST  RXIT CALL RETURN CODE
WKCALLPT DS    ad                 RETURNED RECORD POINTER
WKCALLRL DS    F                  RETURNED RECORD LENGTH
*
WKRPTDCB DS    A                  CONTROL   REPORT      "DCB"
*
         DS    0F
OUTDCB   DS    XL(outfilel)    Reentrant DCB and DCBE areas
*
WKWTOPRM WTO   TEXT=(R2),MF=L
WKWTOLEN EQU   *-WKWTOPRM
*
*WK_MSG   GVBMSG PREFIX=WMSG,MF=L
*
         DS   0A
WKTXTBUF DS    CL135              Message buffer
WKTXTLEN DS    0HL002
WKPRTTXT DS    CL133
*
ADAPAL   DS   0F
         DS  11F
*
LINKWORK DS    XL256
APLXLINK DS    F
WKCID    DS   0F
WKCID1   DS    X
WKCID23  DS    XL2
WKCID4   DS    X
WKAADA   DS    F
WKRETC   DS    F
*
CB       DS    XL(ACBXQLL)
*
FB       DS    XL(FBDXQLL)
         DS    CL56
*
SB       DS    XL(SBDXQLL)
         DS    CL56
*
VB       DS    XL(VBDXQLL)
         DS    CL56
*
IB       DS    XL(IBDXQLL)
         DS    XL100
*
MB       DS    XL(MBDXQLL)
MBCOUNT  DS    F
MBAREA   DS (NREC)CL(MISN)        100 * 16 byte ISN areas
*
RB       DS    XL(RBDXQLL)
RBAREA   DS (NREC)CL(LREC)        100 * 96 byte records
*
WORKLEN  EQU   (*-WORKAREA)
*
LREC     EQU   96
NREC     EQU   100
MISN     EQU   16
*
*
***********************************************************************
*                                                                     *
*        S T A R T U P   P A R A M E T E R   D A T A                  *
*                                                                     *
***********************************************************************
STRTDATA DSECT
*
SDDDNPFX DS    CL04               DDNAME  PREFIX (_PARM, _INIT, _RPT)
         DS    CL28
                        SPACE 3
*
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
EXPREFLN EQU   *-EXTREC
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*     C O M M O N   L A N G U A G E   E N V I R O N M E N T   A R E A *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
LEINTER  DSECT
*
LEPARM   DS   0A
LEFUNCA  DS    A                  FUNCTION CODE
LESUBRA  DS    A                  ENTRY    POINT   ADDRESS
LETOKNA  DS    A                  LANGUAGE ENVIRONMENT TOKEN ADDRESS
LEPARMA  DS    A                  CALLED   MODULE  PARAMETER LIST
LERTNCA  DS    A                  RETURN   CODE
LEREASA  DS    A                  REASON   CODE
LEFDBKA  DS    A                  FEEDBACK AREA
*
LECEEADR DS    A                  LANGUAGE ENVIRONMENT INTERFACE  ADDR
LETOKEN  DS    A                  LANGUAGE ENVIRONMENT TOKEN
LESUBADR DS   2A                  CALLED   ENTRY POINT ADDR   (DBLWORD)
LEPARMP  DS    A                  PARAMETER LIST ADDR
LERTNC   DS    F                  RETURN   CODE
LEREASON DS    F                  REASON   CODE
LERETADR DS    A                  RETURN   ADDRESS
LEFDBK   DS   3A                  FEEDBACK AREA
*
LERDECBA DS    A                  "GENPIPE/GENREAD"    DECB  LIST
LEWDECBA DS    A                  "GENPIPE" PIPE WRITE DECB  LIST
LELRECL  DS    F                  READ EXIT RETURNS
LERECFM  DS    X                  READ EXIT RETURNS
         DS    X                  (FILLER)
*
LEREGSAV DS  18F                  REGISTER  SAVE AREA
*
LEINTLEN EQU   *-LEINTER
*
GLOBAREA DSECT
GLOREC#  DS    F                  TOTAL RECORDS
GLOPARTP DS    F                  PARTITIONS PROCESSED
GLOPARTT DS    F                  PARTITIONS TOTAL
GLOBLEN  EQU   *-GLOBAREA
*
         print off
         SYSSTATE ARCHLVL=2
         COPY  ASMMSP
LEAVE    OPSYN ASM_LEAVE
         asmmrel on
         print on
*
*
***********************************************************************
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
***********************************************************************
*
         YREGS
*
         PRINT nogen
*
GVBXRA   RMODE 24
GVBXRA   AMODE 31
GVBXRA   CSECT
         J     CODE
*RAKEYE  GVBEYES GVBXRA
XRAKEYE  DS    0H
*
static   loctr            set up the static loctr
code     loctr            followed by the code loctr
         using savf4sa,r13          map the save area
         stmg  R14,R12,SAVF4SAG64RS14 save callers registers GVBMR95
*
         llgtr R12,r15            SET   PROGRAM   BASE REGISTERS
         USING (GVBXRA,code),R12
*
         llgtr r9,r1              LOAD  PARAMETER LIST ADDRESS
         USING GENPARM,R9
         llgt  R8,GPWORKA         LOAD  WORK  AREA ANCHOR ADDR
         ltgf  R1,0(,R8)          WORK  AREA  ALLOCATED ???
         JP    CHAIN              YES - CHAIN THEM TOGETHER (RSA'S)
*
         lghi  R0,WORKLEN+8       LOAD   WORK AREA SIZE
         STORAGE OBTAIN,LENGTH=(0),LOC=24,CHECKZERO=YES GET WORKAREA
         ch    r15,=h'14'         not zeroed?
         je    a0002
         lr    r10,r1               save address
         lr    R0,R1                ZERO  WORK  AREA
         lghi  R1,WORKLEN+8
         xr    R14,R14
         xr    R15,R15
         MVCL  R0,R14
         lr    r1,r10               restore pointer
A0002    EQU   *
*
         MVC   0(l'GVBXRAEY,R1),GVBXRAEY
         aghi  r1,l'GVBXRAEY        move pointer past
         drop  r13
         USING WORKAREA,R1
         using savf4sa,workarea
         stg   r13,savf4saprev    save current r13
         mvc   savf4said(4),=a(savf4said_value) set 'F4SA' in area
         ST    R13,WKMR95WA       SAVE GVBMR95  WORK  AREA ADDRESS
         lgr   R13,r1             Get new workarea into r13
         ST    R13,0(,R8)         SAVE  WORK  AREA ADDRESS
*
         BRAS  R10,INITWORK       INITIALIZE WORK AREA
         LTR   R15,R15            Returns r4 => work area
         JNZ   RETURNX
         J     MAINLINE           BEGIN
*
CHAIN    ds    0h
         stg   r13,savf4saprev    save current r13
         llgtr R13,r1             Get new workarea into r13
         drop  r1
         using workarea,r13
         using savf4sa,workarea
         llgt  r4,wkgloba
         using globarea,r4
*
***********************************************************************
MAINLINE DS    0H
***********************************************************************
         XC    WKRETC,WKRETC      Assume Rc=0
*
         LLGT  R8,GPENVA
         USING GENENV,R8
*
         CLC   GPPHASE,=CL2'OP'
         JNE   A0200
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(18),=CL18'GVBXRA: OPEN PHASE'
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
         BRAS  R10,LOADB
         BRAS  R10,UNLOADB
         J     RETURNX
*
A0200    EQU   *
         CLC   GPPHASE,=CL2'CL'
         JNE   A0300
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(19),=CL19'GVBXRA: CLOSE PHASE'
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
         J     RETURNX
*
A0300    EQU   *
         CLC   GPPHASE,=CL2'RD'
         JNE   A0400
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(18),=CL18'GVBXRA: READ PHASE'
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
         BRAS  R10,LOADB
         BRAS  R10,UNLOADB
         J     RETURNX
*
A0400    EQU   *
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(29),=CL29'GVBXRA: UNRECOGNIZED PHASE XX'
         MVC   WKPRINT+27(2),GPPHASE
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
         MVC   WKRETC,=F'16'
         J     RETURNX
*
         DROP  R8
*
***********************************************************************
*   FILL   NEXT "BUFSIZE" buffer                                      *
***********************************************************************
         USING ADACBX,CB
         USING FBBDX,FB
         USING RBBDX,R5
         USING SBBDX,SB
         USING VBBDX,VB
         USING IBBDX,IB
         USING MBBDX,MB
*
LOADB    DS    0H
         ST    R10,WKREG10
         CLI   WKEOF,C'Y'
         JNE   LOADB10
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(57),=CL57'GVBXRA: ALREADY AT END OF FILE. RETURN+
               ING RC=8 TO GVBMR95'
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(35),=CL35'GVBXRA: BLOCKS RETURNED TO GVBMR95 '
         L     R15,WKBUFRET
         CVD   R15,WKDBLWK
         MVC   WKPRINT+35(7),NUMMSK+5
         MVI   WKPRINT+35,C' '
         ED    WKPRINT+35(7),WKDBLWK+5
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
         MVC   WKRETC,=F'8'
         J     LOADB99
*
LOADB10  EQU   *
         CLI   WKREAD1,C'Y'
         JNE   A0013
*
         MVI   WKREAD1,C'N'
*
         MVC   ACBXCMD,=CL2'L3'
         MVI   ACBXCOP1,C'M'
         MVI   ACBXCOP2,C'V'
         MVC   ACBXADD1,=CL8'AA      '
         XC    ACBXISN,ACBXISN
         XC    ACBXISL,ACBXISL
         XC    ACBXISQ,ACBXISQ
*
         MVC   FBDXDATA(21),=CL21'AA,AB,AC,AD,AE,AF,AG.'
         MVC   FBDXSEND+4(4),=F'21'
*
         XC    RBDXSEND,RBDXSEND
         MVC   RBDXRECV+4(4),=A(LREC*NREC)
*
         XC    MBDXSEND,MBDXSEND
         MVC   MBDXRECV+4(4),=A(MISN*NREC)
*
         MVC   SBDXDATA(03),=CL3'AA.'
         MVC   SBDXSEND+4(4),=F'3'
*
         MVC   VBDXDATA(10),=CL10'0000000000'
         MVC   VBDXSEND+4(4),=F'10'
*
A0013    EQU   *
         LA    R1,ADAPAL
         L     R15,WKAADA
         LR    R11,R13
         LAY   R13,WKSUBADA
         BASR  R14,R15
         LR    R13,R11
         CLC   ACBXRSP,=H'3'
         JE    A0015
         CLC   ACBXRSP,=H'0'
         JE    A0014
*        DC    H'0'
         MVC   WKRETC,=F'16'
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(31),=CL31'GVBXRA: Read(1) Error xxxx/xxxx'
         J     A0017
*
A0014    EQU   *
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(18),=CL18'GVBXRA: READ DONE:'
         XC    WKRECBUF,WKRECBUF
         USING MBDSECT,R10
         LAY   R10,MBAREA
         LAY   R11,RBAREA
A001400  EQU   *
         CLC   MBRESP,=F'3'
         JE    A0015
         CLC   MBRESP,=F'0'
         JE    A001402
*        DC    H'0'
         MVC   WKRETC,=F'16'
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(31),=CL31'GVBXRA: Read(2) Error xxxx/xxxx'
         J     A0017
*
A001402  EQU   *
         MVC   WKPRINT+18(LREC),0(R11)
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
         LA    R10,MISN(,R10)
         LA    R11,LREC(,R11)
         ASI   WKRECBUF,1        records in buffer so far
         CLC   WKRECBUF,MBCOUNT  all records retrieved ?
         JL    A001400           no, back for next...
         L     R0,WKRECCNT
         A     R0,WKRECBUF
         ST    R0,WKRECCNT       total records so far
         J     LOADB99           read one block only, i.e. one L3.
* * *    J     A0013             read next block
*
A0015    EQU   *
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(19),=CL19'GVBXRA: END OF DATA'
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(39),=CL39'GVBXRA: Records read in this partition+
               :'
         L     R15,WKRECCNT
         CVD   R15,WKDBLWK
         MVC   WKPRINT+40(7),NUMMSK+5
         MVI   WKPRINT+40,C' '
         ED    WKPRINT+40(7),WKDBLWK+5
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
         L     R0,GLOREC#                Maintain global rec count
         A     R0,WKRECCNT
         ST    R0,GLOREC#
         L     R0,GLOPARTP               Maintain global part count
         A     R0,=F'1'
         ST    R0,GLOPARTP
*
         CLC   GLOPARTP,GLOPARTT
         JL    A0015A
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(39),=CL39'GVBXRA: Records read in all partitions+
               :'
         L     R15,GLOREC#
         CVD   R15,WKDBLWK
         MVC   WKPRINT+40(7),NUMMSK+5
         MVI   WKPRINT+40,C' '
         ED    WKPRINT+40(7),WKDBLWK+5
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(39),=CL39'GVBXRA: Number of partitions processed+
               :'
         L     R15,GLOPARTP
         CVD   R15,WKDBLWK
         MVC   WKPRINT+40(7),NUMMSK+5
         MVI   WKPRINT+40,C' '
         ED    WKPRINT+40(7),WKDBLWK+5
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
A0015A   EQU   *
         MVC   ACBXCMD,=CL2'CL'
         XC    FBDXSEND,FBDXSEND
         XC    RBDXSEND,RBDXSEND
         XC    SBDXSEND,SBDXSEND
         XC    VBDXSEND,VBDXSEND
         XC    RBDXRECV,RBDXRECV
         LA    R1,ADAPAL
         L     R15,WKAADA
         LR    R11,R13
         LAY   R13,WKSUBADA
         BASR  R14,R15
         LR    R13,R11
         CLC   ACBXRSP,=H'0'
         JE    A0015B
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(31),=CL31'GVBXRA: Close Error xxxx/xxxx  '
         J     A0017
A0015B   EQU   *
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(18),=CL18'GVBXRA: CLOSE DONE'
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
         MVI   WKEOF,C'Y'
         J     LOADB99
*
*
A0017    EQU   *
         LH    R15,ACBXRSP
         CVD   R15,WKDBLWK
         MVC   WKPRINT+22(4),NUMMSK+8
         MVI   WKPRINT+22,C' '
         ED    WKPRINT+22(4),WKDBLWK+6
         LH    R15,ACBXERRC
         CVD   R15,WKDBLWK
         MVC   WKPRINT+27(4),NUMMSK+8
         MVI   WKPRINT+27,C' '
         ED    WKPRINT+27(4),WKDBLWK+6
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
*
LOADB99  EQU   *
         L     R10,WKREG10        Return address
         L     R15,WKRETC
         BR    R10
*
***********************************************************************
*   RETURN NEXT "BUFSIZE" buffer                                      *
***********************************************************************
UNLOADB  DS    0H
         LLGT  R15,=A(LREC)
         MS    R15,WKRECBUF
*
         LLGT  R14,GPBLKSIZ
         USING GENBLKSZ,R14
         ST    R15,GP_RESULT_BLK_SIZE
         DROP  R14
*
         LAY   R1,RBAREA
         LLGTR R1,R1
         LLGT  R14,GPBLOCKA       LOAD  POINTER ADDRESS
         USING GENBLOCK,R14
         STG   R1,GP_RESULT_PTR   RETURN BLOCK ADDRESS
         DROP  R14
*
         LTR   R15,R15
         JZ    U0002
         XC    WKRECBUF,WKRECBUF  RESET BUFFER COUNT = 0
         ASI   WKBUFRET,1         INCREMENT NUMBER BUFFERS RETURNED
         J     U0004
U0002    EQU   *
         MVC   WKRETC,=F'8'       ALREADY EOF
U0004    EQU   *
*
         BR    R10                RETURN
*
***********************************************************************
*   RETURN TO GVBMR95                                                 *
***********************************************************************
RETURNX  DS    0H
         LLGT  R15,WKRETC
         LLGT  R14,GPRTNCA        INDICATE NORMAL COMPLETION
         USING GENRTNC,R14
         ST    R15,GP_RETURN_CODE
         DROP  R14
*
         lg    r13,savf4saprev         restore caller save area
         lg    r14,SAVF4SAG64RS14      restore caller's r14
         lmg   r0,r12,SAVF4SAG64RS0    restore caller's r2- r12
         BSM   0,R14              RETURN
*
* NOT CURRENTLY USED -------------------------------------------------
*
RTNERROR ds    0h
         LLGT  R9,WKPLISTA
         USING GENPARM,R9
*
*
RTNERR_2 llgt  R3,GPENVA          LOAD   ENVIRONMENT INFO   ADDRESS
         USING GENENV,R3
*
         LH    R15,WKTXTBUF       RETURN TEXT LENGTH
         ST    R15,GP_ERROR_BUFFER_LEN
*
         BCTR  R15,0              DECREMENT   LENGTH FOR  "EX"
         LLGT  R14,GP_ERROR_BUFFER_PTR ERROR MESSAGE BUFFER ADDRESS
         LA    R1,WKTXTBUF+4
         EXRL  R15,MVCR14R1
*
         LGHI  R15,123            SET REASON CODE
         ST    R15,GP_ERROR_REASON
         J     RETURNX
*
         DROP  R4 GLOBAREA
         DROP  R3 GENENV
*
***********************************************************************
*        I N I T I A L I Z E   W O R K   A R E A                      *
***********************************************************************
*
INITWORK DS    0H
         STMG  R14,R12,WKSAB+8
         ST    R9,WKPLISTA        SAVE  PARAMETER  LIST   ADDRESS
*
         MVC   WKCALLPL(WKCALLEN),0(R9)  COPY READ EXIT PARAM LIST
         LA    R0,WKRXITRC        RETURN     CODE
         ST    R0,WKCALLP8
         LA    R0,WKCALLPT        RETURNED RECORD  POINTER
         ST    R0,WKCALLP9
         LA    R0,WKCALLRL        RETURNED RECORD  LENGTH
         ST    R0,WKCALLPA
*
         LAY   R14,MDLWTO         INITIALIZE  WTO  PARAMETER AREA
         MVC   WKWTOPRM(WKWTOLEN),0(R14)
*
***********************************************************************
*  SAVE DDNAME AND SET EVENT FILE ATTRIBUTES                          *
***********************************************************************
         LLGT  R14,GPFILEA        EVENT FILE DDNAME
         USING GENFILE,R14
         MVC   WKDDNAME,GPDDNAME
         MVC   WKGPLFID,GPLFID
         MVC   GPRECLEN,=A(LREC)
         MVC   GPRECMAX,=A(LREC)
         MVC   GPBLKMAX,=A(LREC*NREC)
         MVI   GPRECFMT,C'F'      FIXED      FORMAT   RECORDS
         XC    GPRECDEL,GPRECDEL
         DROP  R14
*
         LLGT  R14,GPENVA
         USING GENENV,R14
         MVC   WKVIEW#,GPVIEW#
         LLGT  R1,GPENVVA
         MVC   WKENVV,0(R1)
         MVC   WKDATETIME,GP_PROCESS_DATE_TIME
         MVC   WKTHRDNO,GPTHRDNO
         MVC   WKPARTS#,GP_PF_count
         DROP  R14
*
         LLGT  R14,GPSTARTA
         USING GENSTART,R14
         MVC   WKSTART,GP_STARTUP_DATA
         DROP  R14
*
         CLC   WKSTART(6),=CL6'TRACE='
         JNE   I0010
         MVC   WKTRACE,WKSTART+6
I0010    EQU   *
*
***********************************************************************
*  OPEN MESSAGE FILE                                                  *
***********************************************************************
         LA    R14,outfile               COPY MODEL   DCB
d1       using ihadcb,outdcb
         MVC   outdcb(outfilel),0(R14)
         lay   R0,outdcb                 SET  DCBE ADDRESS IN  DCB
         aghi  R0,outfile0
         sty   R0,d1.DCBDCBE
*
         LAY   R2,OUTDCB
         MVC   WKREENT(8),OPENPARM
         OPEN  ((R2),(OUTPUT)),MODE=31,MF=(E,WKREENT)
*                                                                @I1015
*      echo DDNAME
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(32),=CL32'GVBXRA: Being executed with DD:'
         MVC   WKPRINT+32(8),WKDDNAME
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*                                                                @I1015
*      echo THRD#
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(33),=CL33'GVBXRA: Being executed in thread:'
         LH    R15,WKTHRDNO
         CVD   R15,WKDBLWK
         MVC   WKPRINT+34(4),NUMMSK+8
         MVI   WKPRINT+34,C' '
         ED    WKPRINT+34(4),WKDBLWK+6
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*                                                                @I1015
*      echo NUMBER PARTITIONS
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(32),=CL32'GVBXRA: Total number partitions:'
         L     R15,WKPARTS#
         CVD   R15,WKDBLWK
         MVC   WKPRINT+33(4),NUMMSK+8
         MVI   WKPRINT+33,C' '
         ED    WKPRINT+33(4),WKDBLWK+6
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*                                                                @I1015
*      echo VIEW#
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(33),=CL33'GVBXRA: Being executed with VIEW:'
         L     R15,WKVIEW#
         CVD   R15,WKDBLWK
         MVC   WKPRINT+34(7),NUMMSK+5
         MVI   WKPRINT+34,C' '
         ED    WKPRINT+34(7),WKDBLWK+5
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*                                                                @I1015
*      echo GPLFID
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(34),=CL34'GVBXRA: Being executed with LFID:'
         L     R15,WKGPLFID
         CVD   R15,WKDBLWK
         MVC   WKPRINT+34(7),NUMMSK+5
         MVI   WKPRINT+34,C' '
         ED    WKPRINT+34(7),WKDBLWK+5
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*                                                                @I1015
*      echo MR95ENVV
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(38),=CL38'GVBXRA: Being executed with MR95ENVV:'
         MVC   WKPRINT+38(32),WKENVV
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*                                                                @I1015
*      echo DATETIME
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(33),=CL33'GVBXRA: Being executed DateTime:'
         MVC   WKPRINT+33(16),WKDATETIME
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*                                                                @I1015
*      echo GP_STARTUP_DATA
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(37),=CL37'GVBXRA: Being executed with startup:'
         MVC   WKPRINT+37(32),WKSTART
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
***********************************************************************
*  OPEN INPUT FILE (ADABAS)                                           *
***********************************************************************
*
         LAY   R3,CB
         USING ADACBX,CB
         LAY   R4,FB
         USING FBBDX,FB
         LAY   R5,RB
         USING RBBDX,R5
         LAY   R6,SB
         USING SBBDX,SB
         LAY   R7,VB
         USING VBBDX,VB
         LAY   R8,IB
         USING IBBDX,IB
         LAY   R9,MB
         USING MBBDX,MB
*
         ST    R3,ADAPAL+00
         LA    R0,APLXLINK
         ST    R0,ADAPAL+04
         LA    R0,LINKWORK
         ST    R0,ADAPAL+08
         STM   R4,R9,ADAPAL+12
         OI    ADAPAL+32,X'80'
*
*
         LOAD  EPLOC=LINKNAME,ERRET=A0010
         OILH  R0,MODE31
         ST    R0,WKAADA
         J     A0011
A0010    EQU   *
         WTO 'GVBXRA: ADABAS LINK MODULE NOT LOADED'
A0011    EQU   *
*
*
         MVI   ACBXVERT,ACBXVERE
         MVI   ACBXVERN,ACBXVERC
         MVC   ACBXLEN,=Y(ACBXQLL)
         MVC   ACBXDBID,DBID300
         MVC   ACBXFNR,FNR0047
*
         MVI   WKCID1,C'F'
         MVC   WKCID23,=X'0001'
         MVI   WKCID4,C'S'
*
         MVI   FBDXLOC,C' '
         MVC   FBDXLEN,=Y(FBDXQLL)
         MVI   FBDXVERT,ABDXVERE
         MVI   FBDXVERN,ABDXVERC
         MVI   FBDXID,ABDXQFB
         MVC   FBDXSIZE+4(4),=F'50'
*
         MVI   RBDXLOC,C' '
         MVC   RBDXLEN,=Y(RBDXQLL)
         MVI   RBDXVERT,ABDXVERE
         MVI   RBDXVERN,ABDXVERC
         MVI   RBDXID,ABDXQRB
         MVC   RBDXSIZE+4(4),=A(LREC*NREC)
*
         MVI   MBDXLOC,C' '
         MVC   MBDXLEN,=Y(MBDXQLL)
         MVI   MBDXVERT,ABDXVERE
         MVI   MBDXVERN,ABDXVERC
         MVI   MBDXID,ABDXQMB
         MVC   MBDXSIZE+4(4),=A(4+MISN*NREC)  QUANTITY THEN MISN AREA
*
         MVI   SBDXLOC,C' '
         MVC   SBDXLEN,=Y(SBDXQLL)
         MVI   SBDXVERT,ABDXVERE
         MVI   SBDXVERN,ABDXVERC
         MVI   SBDXID,ABDXQSB
         MVC   SBDXSIZE+4(4),=F'50'
*
         MVI   VBDXLOC,C' '
         MVC   VBDXLEN,=Y(VBDXQLL)
         MVI   VBDXVERT,ABDXVERE
         MVI   VBDXVERN,ABDXVERC
         MVI   VBDXID,ABDXQVB
         MVC   VBDXSIZE+4(4),=F'50'
*
         MVI   IBDXLOC,C' '
         MVC   IBDXLEN,=Y(IBDXQLL)
         MVI   IBDXVERT,ABDXVERE
         MVI   IBDXVERN,ABDXVERC
         MVI   IBDXID,ABDXQIB
         MVC   IBDXSIZE+4(4),=F'100'
*
*
         MVC   ACBXCMD,=CL2'OP'
         MVC   ACBXCID,WKCID
         MVC   ACBXADD3,SPACES
         MVC   ACBXADD4,SPACES
         MVC   ACBXADD5,SPACES
         MVC   RBDXDATA(07),=CL7'ACC=47.'
         MVC   RBDXSEND+4(4),=F'7'
         LA    R1,ADAPAL
         L     R15,WKAADA
         LR    R11,R13
         LAY   R13,WKSUBADA
         BASR  R14,R15
         LR    R13,R11
*
         CLC   ACBXRSP,=H'0'
         JE    A0012
*        DC    H'0'
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(30),=CL30'GVBXRA: Open Error xxxx/xxxx  '
         LH    R15,ACBXRSP
         CVD   R15,WKDBLWK
         MVC   WKPRINT+19(4),NUMMSK+8
         MVI   WKPRINT+19,C' '
         ED    WKPRINT+19(4),WKDBLWK+6
         LH    R15,ACBXERRC
         CVD   R15,WKDBLWK
         MVC   WKPRINT+24(4),NUMMSK+8
         MVI   WKPRINT+24,C' '
         ED    WKPRINT+24(4),WKDBLWK+6
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
         MVC   WKRETC,=F'16'
         J     I0099
*
A0012    EQU   *
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(18),=CL18'GVBXRA: OPEN DONE '
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
         MVI   WKREAD1,C'Y'
*
*
***********************************************************************
* ENQ FOR DEALING WITH SHAREAD GLOBAL AREA                            *
***********************************************************************
INITENQ  MVC   REENTWK(MDLENQXL),MDLENQX
         ENQ   MF=(E,REENTWK)
         LTR   R15,R15
         JZ    INITGLOB
         WTO 'GVBXRA: GLOBAL AREA ENQ FAILED'
         MVC   WKRETC,=F'16'
         J     I0099
*
***********************************************************************
* CHECK FOR PRE-EXISTENCE OF GLOBAL AREA                              *
***********************************************************************
INITGLOB EQU   *
         MVC   WKTOKNAM+0(8),GENEVA
         MVC   WKTOKNAM+8(8),TKNNAME
         CALL  IEANTRT,(TOKNLVL2,WKTOKNAM,WKTOKN,WKTOKNRC),            X
               MF=(E,REENTWK)
         LTGF  R15,WKTOKNRC       SUCCESSFUL   ???
         JNZ   INITGLBL           NO  - CREATE THE FIRST ONE
         LLGT  R4,WKTOKNGB        RETRIEVE  GLOBAL AREA  ADDRESS
         ST    R4,WKGLOBA
         USING GLOBAREA,R4
         J     INITDEQ
         DROP  R4 GLOBAREA
***********************************************************************
* INITIALIZE GLOBAL AREA MEMORY                                       *
***********************************************************************
INITGLBL EQU   *
         XC    WKTOKN,WKTOKN
         LHI   R0,GLOBLEN+8
         GETMAIN RU,LV=(0),LOC=(ANY)
         MVC   0(8,R1),GLOBEYEB
         AHI   R1,8
         ST    R1,WKGLOBA
         ST    R1,WKTOKNGB
         LR    R4,R1
         USING GLOBAREA,R4
         XC    0(GLOBLEN,R4),0(R4)  ZERO GLOBAL AREA
         MVC   GLOPARTT,WKPARTS#    SET TOTAL NUMBER PARTITIONS
         DROP  R4 GLOBAREA
***********************************************************************
*  CREATE GLOBAL NAME/TOKEN AREA                                      *
***********************************************************************
         CALL  IEANTCR,(TOKNLVL2,WKTOKNAM,WKTOKN,TOKNPERS,WKTOKNRC),   +
               MF=(E,REENTWK)
         LTGF  R15,WKTOKNRC       SUCCESSFUL  ???
         JZ    INITDEQ
         WTO 'GVBXRA: GLOBAL AREA CREATE FAILED'
         MVC   WKRETC,=F'16'
***********************************************************************
* DEQ FOR DEALING WITH SHAREAD GLOBAL AREA                            *
***********************************************************************
INITDEQ  MVC   REENTWK(MDLDEQXL),MDLDEQX
         DEQ   MF=(E,REENTWK)
*
*
I0099    EQU   *
         LMG   R14,R12,WKSAB+8
         L     R15,WKRETC
         BR    R10                RETURN
*
         DROP  R9
*
STATIC   LOCTR
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         ds    0d
MVCR14R1 MVC   0(0,R14),0(R1)     * * * * E X E C U T E D * * * *
         ds    0d
CLCR1R14 CLC   0(0,R1),0(R14)     * * * * E X E C U T E D * * * *
*
         DS   0D
MODE31   equ   X'8000'
         DS   0D
OPENPARM DC    XL8'8000000000000000'
*
OP       DC    CL2'OP'
CL       DC    CL2'CL'
TOKNPERS DC    F'0'                    TOKEN PERSISTENCE
TOKNLVL1 DC    A(1)                    NAME/TOKEN  AVAILABILITY  LEVEL
TOKNLVL2 DC    A(2)                    NAME/TOKEN  AVAILABILITY  LEVEL
GLOBEYEB DC    CL8'GLOBAREA'
GVBXRAEY DC    CL8'GVBXRA'
GENEVA   DC    CL8'GENEVA'
TKNNAME  DC    CL8'GVBXRGB'
LPGMNAME DS   0CL8                     MINOR  ENQ  NAME (PROGRAM)
EXTNAME  DC    CL128'GVBXRG'           MINOR  ENQ  NAME (EXTENSION)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D A T A   C O N T R O L   B L O C K S                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
outfile  DCB   DSORG=PS,DDNAME=SYSPRINT,MACRF=(PM),DCBE=outfdcbe,      x
               RECFM=FB,LRECL=131
outfile0 EQU   *-outfile
outfdcbe DCBE  RMODE31=BUFF
outfilel EQU   *-outfile
*
RPTFILE  DCB   DSORG=PS,DDNAME=MERGRPT,MACRF=(PM),DCBE=RPTDCBE,        X
               RECFM=FBA,LRECL=133
RPTEOFF  EQU   *-RPTFILE
RPTDCBE  DCBE  RMODE31=BUFF
RPTFILEL EQU   *-RPTFILE
*
MDLENQP  ENQ   (GENEVA,LPGMNAME,E,,STEP),RNL=NO,MF=L
MDLENQPL EQU   *-MDLENQP
*
MDLDEQP  DEQ   (GENEVA,LPGMNAME,,STEP),RNL=NO,MF=L
MDLDEQPL EQU   *-MDLDEQP
*
MDLENQX  ENQ   (GENEVA,EXTNAME,E,,STEP),RNL=NO,MF=L
MDLENQXL EQU   *-MDLENQX
*
MDLDEQX  DEQ   (GENEVA,EXTNAME,,STEP),RNL=NO,MF=L
MDLDEQXL EQU   *-MDLDEQX
*
MDLWTO   WTO   TEXT=(R2),MF=L     MODEL WTO TO DISPLAY CONSOLE MESSAGES
*
ASMADAEY DC    CL8'ASMADAEY'
DBID300  DC    F'300'
FNR0047  DC    F'47'
LINKNAME DC    CL8'ADAUSER'
SPACES   DC    CL256' '
XHEXFF   DC 1024X'FF'
*
         LTORG ,
*
NUMMSK   DC    XL12'402020202020202020202021'
*
*******************************************************
*                 UNPACKED NUMERIC TRANSLATION MATRIX
*******************************************************
*                    0 1 2 3 4 5 6 7 8 9 A B C D E F
*
TRTTBLU  DC    XL16'08080808080808080808080808080808'  00-0F
         DC    XL16'08080808080808080808080808080808'  10-1F
         DC    XL16'08080808080808080808080808080808'  20-2F
         DC    XL16'08080808080808080808080808080808'  30-3F
         DC    XL16'08080808080808080808080808080808'  40-4F
         DC    XL16'08080808080808080808080808080808'  50-5F
         DC    XL16'08080808080808080808080808080808'  60-6F
         DC    XL16'08080808080808080808080808080808'  70-7F
         DC    XL16'08080808080808080808080808080808'  80-8F
         DC    XL16'08080808080808080808080808080808'  90-9F
         DC    XL16'08080808080808080808080808080808'  A0-AF
         DC    XL16'08080808080808080808080808080808'  B0-BF
         DC    XL16'08080808080808080808080808080808'  C0-CF
         DC    XL16'08080808080808080808080808080808'  D0-DF
         DC    XL16'08080808080808080808080808080808'  E0-EF
         DC    XL16'00000000000000000000080808080808'  F0-FF
*
         DS   0F
         DCBD  DSORG=PS
*
         IHADCBE
*
JFCBAR   DSECT
         IEFJFCBN LIST=YES
*
         CVT   DSECT=YES
*
         IHAPSA
         END
